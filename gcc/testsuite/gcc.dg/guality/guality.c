/* { dg-do run { xfail { ! aarch64*-*-* } } } */
/* { dg-options "-g" } */
/* { dg-require-effective-target alloca } */

#include "guality.h"

/* Some silly sanity checking.  */

int
main (int argc, char *argv[])
{
  int i = argc+1;
  int j = argc-2;
  int k = 5;

  GUALCHKXPR (argc);
  GUALCHKXPR (i);
  GUALCHKXPR (j);
  GUALCHKXPR (k);
  GUALCHKXPR (&i);
  GUALCHKFLA (argc);
  GUALCHKFLA (i);
  GUALCHKFLA (j);
  GUALCHKXPR (i);
  GUALCHKXPR (j);
  GUALCHKXPRVAL ("k", 5, 1);
  GUALCHKXPRVAL ("0x40", 64, 0);
  /* GUALCHKXPRVAL ("0", 0, 0); *//* XFAIL */
}
