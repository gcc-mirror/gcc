/* Gcbuiltin.c provides access to some math intrinsic functions.

Copyright (C) 2016-2023 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "Gcbuiltin.h"

#include "config.h"
#include "system.h"

#define exp1 2.7182818284590452353602874713526624977572f

double
cbuiltin_sqrt (double x)
{
  return sqrt (x);
}

long double
cbuiltin_sqrtl (long double x)
{
  return sqrtl (x);
}

float
cbuiltin_sqrtf (float x)
{
  return sqrtf (x);
}

double
cbuiltin_exp (double x)
{
  return exp (x);
}

float
cbuiltin_expf (float x)
{
  return expf (x);
}

long double
cbuiltin_expl (long double x)
{
  return expl (x);
}

/* calculcate ln from log.  */

double
cbuiltin_ln (double x)
{
  return log (x) / log (exp1);
}

float
cbuiltin_lnf (float x)
{
  return logf (x) / logf (exp1);
}

long double
cbuiltin_lnl (long double x)
{
  return logl (x) / logl (exp1);
}

double
cbuiltin_sin (double x)
{
  return sin (x);
}

long double
cbuiltin_sinl (long double x)
{
  return sinl (x);
}

float
cbuiltin_sinf (float x)
{
  return sinf (x);
}

double
cbuiltin_cos (double x)
{
  return cos (x);
}

float
cbuiltin_cosf (float x)
{
  return cosf (x);
}

long double
cbuiltin_cosl (long double x)
{
  return cosl (x);
}

double
cbuiltin_tan (double x)
{
  return tan (x);
}

long double
cbuiltin_tanl (long double x)
{
  return tanl (x);
}

float
cbuiltin_tanf (float x)
{
  return tanf (x);
}

double
cbuiltin_arctan (double x)
{
  return atan (x);
}

float
cbuiltin_arctanf (float x)
{
  return atanf (x);
}

long double
arctanl (long double x)
{
  return atanl (x);
}

int
cbuiltin_entier (double x)
{
  return (int)floor (x);
}

int
cbuiltin_entierf (float x)
{
  return (int)floorf (x);
}

int
cbuiltin_entierl (long double x)
{
  return (int)floorl (x);
}
