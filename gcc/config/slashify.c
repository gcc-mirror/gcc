/* Generate a slashified version of the input
   Copyright (C) 2011
   Free Software Foundation, Inc.
   Contributed by Mike Stump <mikestump@comcast.net>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


/* This is a small utility to slashify a source file so that one never
   needs to stare at backslashes.  */

#include <stdio.h>

extern int main (int, char **);

int main (int argc, char **argv) {
  int c,c1;
  int saw_start = 0;
  while ((c=getchar ()) != EOF) {
    if (c != '@') {
      if (saw_start && c == '\n') {
	putchar ('\\');
      }
      putchar (c);
      continue;
    }
    c1=getchar();
    if (c1 == EOF) {
      putchar (c);
      return 0;
    }
    if (!saw_start && c1 == '(') {
      saw_start = 1;
    } else if (saw_start && c1 == ')') {
      saw_start = 0;
    } else {
      putchar (c); 
      putchar (c1); 
    }
  }
  return 0;
}
