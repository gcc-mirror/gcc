/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-Wunused-macros -traditional-cpp" } */

/* Test everything related to -Wunused-macros.

   Source: Neil Booth, 23 Jul 2002.  */

#include "Wunused.h"

#define used1			/* { dg-bogus "used" } */
#define used2			/* { dg-bogus "used" } */
#define used3			/* { dg-bogus "used" } */
#define used4 something		/* { dg-bogus "used" } */

#define unused5			/* { dg-warning "-:used" } */
#define unused6			/* { dg-warning "-:used" } */
#define unused7()		/* { dg-warning "-:used" } */

#if defined used1
#endif
#ifdef used2
#endif
#ifndef used3
#endif
used4

unused7;			/* This does not count as a use.  */

#if 0
unused5				/* This does not count as a use.  */
#endif

#undef unused5
#define unused6
unused6
