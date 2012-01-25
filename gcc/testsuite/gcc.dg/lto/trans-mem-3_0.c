/* { dg-lto-options {{-flto}} } */
/* { dg-lto-do link } */
/* { dg-require-effective-target stdint_types } */

/* Test that we can build one object file with -fgnu-tm
   (trans-mem-3_1.c), but do the final link of all objects without
   -fgnu-tm.  */

int i;
