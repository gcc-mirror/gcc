# Generate an ELF symbol version map a-la Solaris and GNU ld.
#	Contributed by Richard Henderson <rth@cygnus.com>
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 2, or (at your option) any later
# version.
#
# GCC is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
# License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING.  If not, write to the Free
# Software Foundation, 59 Temple Place - Suite 330, Boston MA
# 02111-1307, USA.

BEGIN {
  state = "nm";
}

# Remove comment and blank lines.
/^ *#/ || /^ *$/ {
  next;
}

# We begin with nm input.  Collect the set of symbols that are present
# so that we can not emit them into the final version script -- Solaris
# complains at us if we do.

state == "nm" && /^%%/ {
  state = "ver";
  next;
}

state == "nm" && ($1 == "U" || $2 == "U") {
  next;
}

state == "nm" && NF == 3 {
  def[$3] = 1;
  next;
}

state == "nm" {
  next;
}

# Now we process a simplified variant of the Solaris symbol version
# script.  We have one symbol per line, no semicolons, simple markers
# for beginning and ending each section, and %inherit markers for
# describing version inheritence.  A symbol may appear in more than
# one symbol version, and the last seen takes effect.

NF == 3 && $1 == "%inherit" {
  inherit[$2] = $3;
  next;
}

NF == 2 && $2 == "{" {
  libs[$1] = 1;
  thislib = $1;
  next;
}

$1 == "}" {
  thislib = "";
  next;
}

{
  ver[$1] = thislib;
  next;
}

END {
  for (l in libs)
    output(l);
}

function output(lib) {
  if (done[lib])
    return;
  done[lib] = 1;
  if (inherit[lib])
    output(inherit[lib]);

  printf("%s {\n", lib);
  printf("  global:\n");
  for (sym in ver)
    if ((ver[sym] == lib) && (sym in def))
      printf("\t%s;\n", sym);

  if (inherit[lib])
    printf("} %s;\n", inherit[lib]);
  else
    printf ("\n  local:\n\t*;\n};\n");
}
