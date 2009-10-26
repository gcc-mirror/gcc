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

/* Valid __ea declarations.  */

/* { dg-do compile } */

/* Typedefs.  */
typedef __ea int ea_int_t;
typedef __ea int *ea_int_star_t;
typedef int outer_t;

/* Externs.  */

__ea extern int i1;
extern __ea int i2;
extern int __ea i3;
extern __ea ea_int_t i4;		/* __ea qualifier permitted via typedef.  */
extern int __ea __ea __ea dupe;		/* __ea duplicate permitted directly.  */
extern int __ea *ppu;

/* Pointers.  */
__ea int *i4p;

/* Structs.  */
struct st {
  __ea int *p;
};

/* Variable definitions.  */
__ea int ii0;
int *__ea ii1;
static int __ea ii2;

void
f1 ()
{
  int *spu;
  ppu = (ea_int_t *) spu;
  ppu = (ea_int_star_t) spu;
}

void
f2 ()
{
  int *spu;
  spu = (int *) ppu;
  ppu = (__ea int *) spu;
}

void
f3 ()
{
  int i = sizeof (__ea int);
}

__ea int *f4 (void)
{
  return 0;
}

int f5 (__ea int *parm)
{
  static __ea int local4;
  int tmp = local4;
  local4 = *parm;
  return tmp;
}

static inline __ea void *f6 (__ea void *start)
{
  return 0;
}

void f7 (void)
{
  __ea void *s1;
  auto __ea void *s2;
}

__ea int *f8 (__ea int *x)
{
  register __ea int *y = x;
  __ea int *z = y;
  return z;
}

long long f9 (__ea long long x[2])
{
  return x[0] + x[1];
}

void f10 ()
{
  static __ea outer_t o;
}
