#!/usr/bin/perl -w

# (C) 2013 Free Software Foundation
# Contributed by Tobias Burnus
#
# This script is Free Software, and it can be copied, distributed and
# modified as defined in the GNU General Public License.  A copy of
# its license can be downloaded from http://www.gnu.org/copyleft/gpl.html

use strict;
use File::Basename;


if ($#ARGV != 0 or $ARGV[0] eq "")  {
   my $name = basename($0);

   print "\nUSAGE: `$name` memory.texi\n\n";
   print "Reads GLIBC's manual/memory.texi and extracts the obstacks section\n"
        ."Redirect the output to update GCC's libiberty/obstacks.texi\n\n";
   exit 1;
}

open (IN, "<$ARGV[0]") || die "Cannot open '$ARGV[0]': $!";
my $data = join ("", <IN>);
close (IN);

$data =~ s/.*\@node Obstacks\n/\@node Obstacks\n/s;
$data =~ s/\n\@node [^\n]+\n\@subsection.*/\n/s;

# Add refs to GLIBC
$data =~ s/(\@p?xref{[^}]*)}/$1, , , libc, The GNU C Library Reference Manual}/gs;


# And undo the refs which are in this file
my @nodes = grep /^\@node /, (split /\n/, $data);

foreach my $node (@nodes) {
  $node =~ s/\@node //;
  $node =~ s/,.*//;
  $node =~ s/ / *\n?/g;
  chomp ($node);

  $data =~ s/(\@p?xref{$node), , , libc, The GNU C Library Reference Manual}/$1}/gsi;
}

print $data;
