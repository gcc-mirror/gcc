/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do run } */

/* This string contains embedded nulls which should be preserved.  */
static char x[] = "A string  /* { dg-warning "null.*string" "nulls in string" { target *-*-* } 6 } */

#include <string.h>

int
main ()
{
  return strlen(x) != 8;
}

/* This comment with    preprocessor silently.  */

/* Some random nulls among whitespace to be warned about once.  */
  
#define N 
#if 0 == '#endif

#if 0#endif

/* The null here should act as a whitespace separator.  */
#define X
#if X != 1
#error		/* { dg-bogus "error" "Check X defined OK" } */
#endif

