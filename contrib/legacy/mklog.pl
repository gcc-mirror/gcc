#!/usr/bin/perl
# Copyright (C) 2012-2024 Free Software Foundation, Inc.
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GCC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING.  If not, write to
# the Free Software Foundation, 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

# This script parses a .diff file generated with 'diff -up' or 'diff -cp'
# and adds a skeleton ChangeLog file to the file. It does not try to be
# very smart when parsing function names, but it produces a reasonable
# approximation.
#
# Author: Diego Novillo <dnovillo@google.com> and
#         Cary Coutant <ccoutant@google.com>

use File::Temp;
use File::Copy qw(cp mv);

$date = `date +%Y-%m-%d`; chop ($date);

$dot_mklog_format_msg =
    "The .mklog format is:\n"
    . "NAME = ...\n"
    . "EMAIL = ...\n";

# Create a .mklog to reflect your profile, if necessary.
my $conf = "$ENV{HOME}/.mklog";
if (-f "$conf") {
    open (CONF, "$conf")
	or die "Could not open file '$conf' for reading: $!\n";
    while (<CONF>) {
	if (m/^\s*NAME\s*=\s*(.*?)\s*$/) {
	    $name = $1;
	} elsif (m/^\s*EMAIL\s*=\s*(.*?)\s*$/) {
	    $addr = $1;
	}
    }
    if (!($name && $addr)) {
	die "Could not read .mklog settings.\n"
	    . $dot_mklog_format_msg;
    }
} else {
    $name = `git config user.name`;
    chomp($name);
    $addr = `git config user.email`;
    chomp($addr);

    if (!($name && $addr)) {
	die "Could not read git user.name and user.email settings.\n"
	    . "Please add missing git settings, or create a .mklog file in"
	    . " $ENV{HOME}.\n"
	    . $dot_mklog_format_msg;
    }
}

$gcc_root = $0;
$gcc_root =~ s/[^\\\/]+$/../;

#-----------------------------------------------------------------------------
# Program starts here. You should not need to edit anything below this
# line.
#-----------------------------------------------------------------------------
$inline = 0;
if ($#ARGV == 1 && ("$ARGV[0]" eq "-i" || "$ARGV[0]" eq "--inline")) {
	shift;
	$inline = 1;
} elsif ($#ARGV != 0) {
    $prog = `basename $0`; chop ($prog);
    print <<EOF;
usage: $prog [ -i | --inline ] file.diff

Generate ChangeLog template for file.diff.
It assumes that patch has been created with -up or -cp.
When -i is used, the ChangeLog template is followed by the contents of
file.diff.
When file.diff is -, read standard input.
When -i is used and file.diff is not -, it writes to file.diff, otherwise it
writes to stdout.
EOF
    exit 1;
}

$diff = $ARGV[0];
$dir = `dirname $diff`; chop ($dir);
$basename = `basename $diff`; chop ($basename);
$hdrline = "$date  $name  <$addr>";

sub get_clname ($) {
	return ('ChangeLog', $_[0]) if ($_[0] !~ /[\/\\]/);

	my $dirname = $_[0];
	while ($dirname) {
		my $clname = "$dirname/ChangeLog";
		if (-f "$gcc_root/$clname" || -f "$clname") {
			my $relname = substr ($_[0], length ($dirname) + 1);
			return ($clname, $relname);
		} else {
			$dirname =~ s/[\/\\]?[^\/\\]*$//;
		} 
	}

	return ('Unknown ChangeLog', $_[0]);
}

sub remove_suffixes ($) {
	my $filename = $_[0];
	$filename =~ s/^[ab]\///;
	$filename =~ s/\.jj$//;
	return $filename;
}

sub is_context_hunk_start {
	return @_[0] =~ /^\*\*\*\*\*\** ([a-zA-Z0-9_].*)/;
}

sub is_unified_hunk_start {
	return @_[0] =~ /^@@ .* @@ ([a-zA-Z0-9_].*)/;
}

# Check if line is a top-level declaration.
sub is_top_level {
	my ($function, $is_context_diff) = (@_);
	if (is_unified_hunk_start ($function)
	    || is_context_hunk_start ($function)) {
	    return 1;
	}
	if ($is_context_diff) {
		$function =~ s/^..//;
	} else {
		$function =~ s/^.//;
	}
	return $function && $function !~ /^[\s{#]/;
}

# Read contents of .diff file
open (DFILE, $diff) or die "Could not open file $diff for reading";
chomp (my @diff_lines = <DFILE>);
close (DFILE);

# Array diff_lines is modified by the log generation, so save a copy in
# orig_diff_lines if needed.
if ($inline) {
    @orig_diff_lines = @diff_lines;
}

# For every file in the .diff print all the function names in ChangeLog
# format.
%cl_entries = ();
$change_msg = undef;
$look_for_funs = 0;
$clname = get_clname('');
$line_idx = 0;
foreach (@diff_lines) {
    # Stop processing functions if we found a new file.
	# Remember both left and right names because one may be /dev/null.
    # Don't be fooled by line markers in case of context diff.
    if (!/\*\*\*$/ && /^[+*][+*][+*] +(\S+)/) {
		$left = remove_suffixes ($1);
		$look_for_funs = 0;
	}
    if (!/---$/ && /^--- +(\S+)?/) {
		$right = remove_suffixes ($1);
		$look_for_funs = 0;
	}

	# Check if the body of diff started.
	# We should now have both left and right name,
	# so we can decide filename.

    if ($left && (/^\*{15}/ || /^@@ /)) {
	# If we have not seen any function names in the previous file (ie,
	# $change_msg is empty), we just write out a ':' before starting the next
	# file.
	if ($clname) {
		$cl_entries{$clname} .= $change_msg ? "$change_msg" : ":\n";
	}

	if ($left eq $right) {
		$filename = $left;
	} elsif($left eq '/dev/null') {
		$filename = $right;
	} elsif($right eq '/dev/null') {
		$filename = $left;
	} else {
		my @ldirs = split /[\/\\]/, $left;
		my @rdirs = split /[\/\\]/, $right;

		$filename = '';
		while ((my $l = pop @ldirs) && (my $r = pop @rdirs)) {
			last if ($l ne $r);
			$filename = "$l/$filename";
		}
		$filename =~ s/\/$//;

		if (!$filename) {
			print STDERR "Error: failed to parse diff for $left and $right\n";
			exit 1;
		}
	}
	$left = $right = undef;
	($clname, $relname) = get_clname ($filename);
	$cl_entries{$clname} .= "\t* $relname";
	$change_msg = '';
	$look_for_funs = $filename =~ '\.(c|cpp|C|cc|h|inc|def)$';
    }

    # Context diffs have extra whitespace after first char;
    # remove it to make matching easier.
    if ($is_context_diff) {
      s/^([-+! ]) /\1/;
    }

    # Remember the last line in a diff block that might start
    # a new function.
    if (/^[-+! ]([a-zA-Z0-9_].*)/) {
        $save_fn = $1;
    }

    # Check if file is newly added.
    # Two patterns: for context and unified diff.
    if (/^\*\*\* 0 \*\*\*\*/
        || /^@@ -0,0 \+1.* @@/) {
        $change_msg = $filename =~ /testsuite.*(?<!\.exp)$/ ? ": New test.\n" : ": New file.\n";
        $look_for_funs = 0;
    }

    # Check if file was removed.
    # Two patterns: for context and unified diff.
    if (/^--- 0 ----/
        || /^@@ -1.* \+0,0 @@/) {
        $change_msg = ": Remove.\n";
        $look_for_funs = 0;
    }

    if (is_unified_hunk_start ($diff_lines[$line_idx])) {
        $is_context_diff = 0;
    }
    elsif (is_context_hunk_start ($diff_lines[$line_idx])) {
	    $is_context_diff = 1;
    }

    # If we find a new function, print it in brackets.  Special case if
    # this is the first function in a file.  
    #
    # Note that we don't try too hard to find good matches.  This should
    # return a superset of the actual set of functions in the .diff file.
    #
    # The first pattern works with context diff files (diff -c). The
    # second pattern works with unified diff files (diff -u).
    #
    # The third pattern looks for the starts of functions or classes
    # within a diff block both for context and unified diff files.
    if ($look_for_funs
        && (/^\*\*\*\*\*\** ([a-zA-Z0-9_].*)/
	|| /^@@ .* @@ ([a-zA-Z0-9_].*)/
	|| /^[-+! ](\{)/))
      {
	$_ = $1;
	my $fn;
	if (/^\{/) {
	    # Beginning of a new function.
	    $_ = $save_fn;
	} else {
	    $save_fn = "";
	}
	if (/;$/) {
	    # No usable function name found.
	} elsif (/^((class|struct|union|enum) [a-zA-Z0-9_]+)/) {
	    # Discard stuff after the class/struct/etc. tag.
	    $fn = $1;
	} elsif (/([a-zA-Z0-9_][^(]*)\(/) {
	    # Discard template and function parameters.
	    $fn = $1;
	    1 while ($fn =~ s/<[^<>]*>//);
	    $fn =~ s/[ \t]*$//;
	}
	# Check is function really modified
	$no_real_change = 0;
	$idx = $line_idx;
	# Skip line info in context diffs.
	while ($idx <= $#diff_lines && $is_context_diff
               && $diff_lines[$idx + 1] =~ /^[-\*]{3} [0-9]/) {
		++$idx;
	}
	# Check all lines till the first change
	# for the presence of really changed function
	do {
		++$idx;
		$no_real_change = $idx > $#diff_lines
				  || is_top_level ($diff_lines[$idx], $is_context_diff);
	} while (!$no_real_change && ($diff_lines[$idx] !~ /^[-+!]/));
	if ($fn && !$seen_names{$fn} && !$no_real_change) {
	    # If this is the first function in the file, we display it next
	    # to the filename, so we need an extra space before the opening
	    # brace.
	    if (!$change_msg) {
		$change_msg .= " ";
	    } else {
		$change_msg .= "\t";
	    }

		$change_msg .= "($fn):\n";
	    $seen_names{$fn} = 1;
	}
    }
	$line_idx++;
}

# If we have not seen any function names (ie, $change_msg is empty), we just
# write out a ':'. This happens when there is only one file with no
# functions.
$cl_entries{$clname} .= $change_msg ? "$change_msg\n" : ":\n";

if ($inline && $diff ne "-") {
	# Get a temp filename, rather than an open filehandle, because we use
	# the open to truncate.
	$tmp = mktemp("tmp.XXXXXXXX") or die "Could not create temp file: $!";

	# Copy the permissions to the temp file (in File::Copy module version
	# 2.15 and later).
	cp $diff, $tmp or die "Could not copy patch file to temp file: $!";

	# Open the temp file, clearing contents.
	open (OUTPUTFILE, '>', $tmp) or die "Could not open temp file: $!";
} else {
	*OUTPUTFILE = STDOUT;
}

# Print the log
foreach my $clname (keys %cl_entries) {
	print OUTPUTFILE "$clname:\n\n$hdrline\n\n$cl_entries{$clname}\n";
}

if ($inline) {
	# Append the patch to the log
	foreach (@orig_diff_lines) {
		print OUTPUTFILE "$_\n";
	}
}

if ($inline && $diff ne "-") {
	# Close $tmp
	close(OUTPUTFILE);

	# Write new contents to $diff atomically
	mv $tmp, $diff or die "Could not move temp file to patch file: $!";
}

exit 0;
