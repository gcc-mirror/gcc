#!/usr/bin/env perl
#
# Copyright (C) 2022-2025 Free Software Foundation, Inc.
# Contributed by Arsen Arsenović.
#
# This script is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# This script reads program output on STDIN, and out of it produces a block of
# dg-output lines that can be yanked at the end of a file.  It will escape
# special ARE and Tcl constructs automatically.
#
# Each argument passed on the standard input is treated as a string to be
# replaced by ``.*'' in the final result.  This is intended to mask out build
# paths, filenames, etc.
#
# Usage example:

# $ g++-13 -fcontracts -o test \
#  'g++.dg/contracts/contracts-access1.C' && \
#   ./test |& dg-out-generator.pl 'g++.dg/contracts/contracts-access1.C'
# // { dg-output {contract violation in function Base::b at .*:11: pub > 0(\n|\r\n|\r)} }
# // { dg-output {\[level:default, role:default, continuation mode:never\](\n|\r\n|\r)} }
# // { dg-output {terminate called without an active exception(\n|\r\n|\r)} }

# You can now freely dump the above into your testcase.

use strict;
use warnings;
use POSIX 'floor';

my $escapees = '(' . join ('|', map { quotemeta } @ARGV) . ')';

sub gboundary($)
{
  my $str = shift;
  my $sz = 10.0;
  for (;;)
    {
      my $bnd = join '', (map chr 64 + rand 27, 1 .. floor $sz);
      return $bnd unless index ($str, $bnd) >= 0;
      $sz += 0.1;
    }
}

while (<STDIN>)
  {
    # Escape our escapees.
    my $boundary;
    if (@ARGV) {
      # Checking this is necessary to avoid a spurious .* between all
      # characters if no arguments are passed.
      $boundary = gboundary $_;
      s/$escapees/$boundary/g;
    }

    # Quote stuff special in Tcl ARE.  This step also effectively nulls any
    # concern about escaping.  As long as all curly braces are escaped, the
    # string will, when passing through the braces rule of Tcl, be identical to
    # the input.
    s/([[\]*+?{}()\\])/\\$1/g;

    # Newlines should be more tolerant.
    s/\n$/(\\n|\\r\\n|\\r)/;

    # Then split out the boundary, replacing it with .*.
    s/$boundary/.*/g if defined $boundary;

    # Then, let's print it in a dg-output block.  If you'd prefer /* keep in
    # mind that if your string contains */ it could terminate the comment
    # early.  Maybe add an extra s!\*/!*()/!g or something.
    print "// { dg-output {$_} }\n";
  }

# File Local Vars:
# indent-tabs-mode: nil
# End:
