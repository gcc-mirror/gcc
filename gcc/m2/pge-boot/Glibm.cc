/* Glibm.c provides access to some libm functions.

Copyright (C) 2016-2025 Free Software Foundation, Inc.
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

#define _libm_C
#include "config.h"
#include "system.h"

#include "Glibm.h"

double
libm_pow (double x, double y)
{
  return pow (x, y);
}

float
libm_powf (float x, float y)
{
  return powf (x, y);
}

long double
libm_powl (long double x, long double y)
{
  return powl (x, y);
}

double
libm_sqrt (double x)
{
  return sqrt (x);
}

float
libm_sqrtf (float x)
{
  return sqrtf (x);
}

long double
libm_sqrtl (long double x)
{
  return sqrtl (x);
}

double
libm_asin (double x)
{
  return asin (x);
}

float
libm_asinf (float x)
{
  return asinf (x);
}

long double
libm_asinl (long double x)
{
  return asinl (x);
}

double
libm_atan (double x)
{
  return atan (x);
}

float
libm_atanf (float x)
{
  return atanf (x);
}

long double
libm_atanl (long double x)
{
  return atanl (x);
}

double
libm_atan2 (double x, double y)
{
  return atan2 (x, y);
}

float
libm_atan2f (float x, float y)
{
  return atan2f (x, y);
}

long double
libm_atan2l (long double x, long double y)
{
  return atan2l (x, y);
}

double
libm_sin (double x)
{
  return sin (x);
}

float
libm_sinf (float x)
{
  return sinf (x);
}

long double
libm_sinl (long double x)
{
  return sinl (x);
}

double
libm_cos (double x)
{
  return cos (x);
}

float
libm_cosf (float x)
{
  return cosf (x);
}

long double
libm_cosl (long double x)
{
  return cosl (x);
}

double
libm_tan (double x)
{
  return tan (x);
}

float
libm_tanf (float x)
{
  return tanf (x);
}

long double
libm_tanl (long double x)
{
  return tanl (x);
}

float
libm_floorf (float x)
{
  return floorf (x);
}

double
libm_floor (double x)
{
  return floor (x);
}

long double
libm_floorl (long double x)
{
  return floorl (x);
}

float
libm_expf (float x)
{
  return expf (x);
}

double
libm_exp (double x)
{
  return exp (x);
}

long double
libm_expl (long double x)
{
  return expl (x);
}

float
libm_logf (float x)
{
  return logf (x);
}

double
libm_log (double x)
{
  return log (x);
}

long double
libm_logl (long double x)
{
  return logl (x);
}
