/* Copyright (C) 2005, 2006 Free Software Foundation, Inc. */
/* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. */

void exit (int);

typedef struct {
  int tag;
  union {
    struct {
      int foo;
      int bar;
      union {
	int bt;
	int bf;
      } inner;
    } first;
    int an;
  } that;
  int final;
} this;

void assert (int v)
{
  if (! v)
    exit(1);
}

void d_test (this *s, int n, int v)
{
  switch (n) {

  case 1: assert(s->tag == v); break;
  case 2: assert(s->that.first.foo == v); break;
  case 3: assert(s->that.first.bar == v); break;
  case 4: assert(s->that.first.inner.bt == v); break;
  case 5: assert(s->that.first.inner.bf == v); break;
  case 6: assert(s->that.an == v); break;
  case 7: assert(s->final == v); break;
  }
}

