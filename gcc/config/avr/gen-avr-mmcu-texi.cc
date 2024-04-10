/* Copyright (C) 2012-2024 Free Software Foundation, Inc.
   Contributed by Georg-Johann Lay (avr@gjlay.de)

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.
   
   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define IN_GEN_AVR_MMCU_TEXI

#define ARRAY_SIZE(a) (sizeof (a) / sizeof ((a)[0]))

#include "avr-devices.cc"

static const avr_mcu_t*
mcus[ARRAY_SIZE (avr_mcu_types)];

static int letter (char c)
{
  return c >= 'a' && c <= 'z';
}

static int digit (char c)
{
  return c >= '0' && c <= '9';
}

static int
str_prefix_p (const char *str, const char *prefix)
{
  return strncmp (str, prefix, strlen (prefix)) == 0;
}


/* Used by string comparator to group MCUs by their
   name prefix like "attiny" or "atmega".  */

static int
c_prefix (const char *str)
{
  static const char *const prefixes[] =
    {
      "attiny", "atmega", "atxmega", "ata", "at90", "avr"
    };

  int i, n = (int) (ARRAY_SIZE (prefixes));

  for (i = 0; i < n; i++)
    if (str_prefix_p (str, prefixes[i]))
      return i;

  return n;
}


/* If A starts a group of digits, return their value as a number.  */

static int
c_number (const char *a)
{
  int val = 0;

  if (digit (*a) && ! digit (*(a-1)))
    {
      while (digit (*a))
	val = 10 * val + (*a++) - '0';
    }

  return val;
}


/* Compare two MCUs and order them for easy lookup.  */

static int
comparator (const void *va, const void *vb)
{
  const avr_mcu_t *mcu_a = *(const avr_mcu_t* const*) va;
  const avr_mcu_t *mcu_b = *(const avr_mcu_t* const*) vb;
  const char *a = mcu_a->name;
  const char *b = mcu_b->name;

  // First, group MCUs according to their pure-letter prefix.

  int c = c_prefix (a) - c_prefix (b);
  if (c)
    return c;

  // Second, if their prefixes are the same, group according to
  // their flash size.

  c = (int) mcu_a->flash_size - (int) mcu_b->flash_size;
  if (c)
    return c;

  // Third, group according to aligned groups of digits.

  while (*a && *b)
    {
      c = c_number (a) - c_number (b);
      if (c)
	return c;

      if (*a != *b)
	return *a - *b;

      a++;
      b++;
    }

  return *a - *b;
}

static void
print_mcus (size_t n_mcus)
{
  int duplicate = 0;
  size_t i;

  if (!n_mcus)
    return;

  qsort (mcus, n_mcus, sizeof (avr_mcu_t*), comparator);

  printf ("@*@var{mcu}@tie{}=");

  for (i = 0; i < n_mcus; i++)
    {
      printf (" @code{%s}%s", mcus[i]->name, i == n_mcus-1 ? ".\n\n" : ",");

      if (i && !strcmp (mcus[i]->name, mcus[i-1]->name))
	{
	  // Sanity-check: Fail on devices that are present more than once.

	  duplicate = 1;
	  fprintf (stderr, "error: duplicate device: %s\n", mcus[i]->name);
	}
    }

  if (duplicate)
    exit (1);
}

int main (void)
{
  enum avr_arch_id arch_id = ARCH_UNKNOWN;
  size_t i, n_mcus = 0;
  const avr_mcu_t *mcu;

  printf ("@c Copyright (C) 2012-2024 Free Software Foundation, Inc.\n");
  printf ("@c This is part of the GCC manual.\n");
  printf ("@c For copying conditions, see the file "
	  "gcc/doc/include/fdl.texi.\n\n");

  printf ("@c This file is generated automatically using\n");
  printf ("@c gcc/config/avr/gen-avr-mmcu-texi.cc from:\n");
  printf ("@c	 gcc/config/avr/avr-arch.h\n");
  printf ("@c	 gcc/config/avr/avr-devices.cc\n");
  printf ("@c	 gcc/config/avr/avr-mcus.def\n\n");

  printf ("@c Please do not edit manually.\n\n");

  printf ("@table @code\n\n");

  for (mcu = avr_mcu_types; mcu->name; mcu++)
    {
      if (mcu->macro == NULL)
	{
	  arch_id = mcu->arch_id;

	  // Start a new architecture:	Flush the MCUs collected so far.
	  print_mcus (n_mcus);
	  n_mcus = 0;

	  for (i = 0; i < ARRAY_SIZE (avr_texinfo); i++)
	    if (arch_id == avr_texinfo[i].arch_id)
	      printf ("@item @anchor{%s}%s\n%s\n", mcu->name, mcu->name,
		      avr_texinfo[i].texinfo);
	}
      else if (arch_id == (enum avr_arch_id) mcu->arch_id)
	{
	  mcus[n_mcus++] = mcu;
	}
    }

  print_mcus (n_mcus);
  printf ("@end table\n");

  return EXIT_SUCCESS;
}
