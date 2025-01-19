#!/usr/bin/perl

# Copy log files from a GCC build for HTTP access.
# Copyright (C) 2008-2025 Free Software Foundation, Inc.
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# INPUT:
# mkindex.pl <srcdir> <destdir> <branchname>

# This script copies log files from a GCC build directory, compresses
# and indexes them for web browser access.  It's aimed at having an
# easy-to-access collection of files for analyzing regressions without
# needing to run the build yourself.  Binary files (.o, executables)
# are intentionally not included since usually if they are needed it's
# better to just run a build, and because they take up a lot of space.

# 'srcdir' is the root directory of a GCC build (was $objdir in the build).
# 'destdir' will be erased and replaced with the log files, and should be an
#   absolute path.
# 'branchname' is used only to produce the title of the index page,
#   which will be named 'index.html'.

use warnings;
use strict;
use File::Path qw(mkpath rmtree);
use File::Find qw(find);

if ($#ARGV != 2) {
    print "usage: $0 <srcdir> <destdir> <branchname>\n";
    exit 1;
}

my ($srcdir, $destdir, $branchname) = @ARGV;
die "destdir is not absolute" unless ($destdir =~ m,^/,);

# Erase the destination.
rmtree $destdir;
mkdir $destdir or die "${destdir}: $!";

# Copy and compress the files into the destination, and keep a list in @files.
my @files = ();
sub my_wanted {
    # Copy all files ending with .log or .sum.
    if (/\.(log|sum)$/ && -f) {

	die unless (substr ($File::Find::dir,0,(length $srcdir)) eq $srcdir);
	my $dir = substr $File::Find::dir,(length $srcdir);
	$dir = substr $dir,1 unless ($dir eq '');
	my $name = $_;
	$name = $dir . '/' . $_ if ($dir ne '');

	mkpath $destdir . '/' . $dir;
	# Compress the files.  Use .gzip instead of .gz for the
	# extension to avoid (broken) browser workarounds for broken
	# web servers.
	system ("gzip -c -q -9 $_ > $destdir/${name}.gzip") == 0 or exit 2;

	# Write the (compressed) size consistently in Kbytes.
	my $size = -s $destdir .'/' . $name . '.gzip';
	my $printable_size = (sprintf "%.0fK",$size / 1024);

	push @files,[$name.'.gzip',$name,$printable_size];
    }
}
find ({wanted => \&my_wanted}, $srcdir);

# Sort the list of files for the index.
@files = sort {$a->[1] cmp $b->[1]} @files;

# Create the index.
open INDEX,'>',$destdir . '/index.html' or die "${destdir}/index.html: $!";
# Use strict XHTML 1.0, and set charset to UTF-8.
print INDEX <<EOF or die "writing index: $!";
<!DOCTYPE html
     PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
     "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
 <title>Log files for $branchname</title>
 <meta http-equiv="Content-Type" content="text/html;charset=utf-8" />
</head>
<body>
<h1>Log files for $branchname</h1>
<table><tr><th>Name</th><th align='right'>Size</th></tr>
EOF
# The index will have two columns, filename (without .gzip) and
# compressed size.
foreach my $f (@files) {
    printf INDEX "<tr><td><a href=\"%s\">%s</a></td><td align=\'right\'>%s</td></tr>\n",
	$f->[0], $f->[1], $f->[2] or die "writing index: $!";
}

print INDEX "</table></body></html>\n" or die "writing index: $!";
close INDEX or die "writing index: $!";
exit 0;
