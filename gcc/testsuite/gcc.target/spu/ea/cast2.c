/* Copyright (C) 2009 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with this file; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* { dg-do run } */

extern void abort (void);

int array[128];

__ea int *ea;
int *lm;

void verify_ea (void) __attribute__ ((noinline));
void
verify_ea (void)
{
  if (ea != (__ea int *)lm)
    abort ();
}

void verify_lm (void) __attribute__ ((noinline));
void
verify_lm (void)
{
  if ((int *)ea != lm)
    abort ();
}

void verify_diff (int x) __attribute__ ((noinline));
void
verify_diff (int x)
{
  if (ea - lm != x)
    abort ();
}

int
main (int argc, char **argv)
{
  ea = 0;
  lm = 0;
  verify_ea ();
  verify_lm ();
  verify_diff (0);

  ea = &array[64];
  lm = &array[64];
  verify_ea ();
  verify_lm ();
  verify_diff (0);

  ea = &array[0];
  lm = &array[64];
  verify_diff (-64);

  ea = &array[64];
  lm = &array[0];
  verify_diff (64);

  return 0;
}
