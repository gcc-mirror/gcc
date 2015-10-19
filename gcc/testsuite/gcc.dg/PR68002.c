/* Ensure static functions can be kept.  */
/* { dg-do compile } */
/* { dg-options "-O1 -fkeep-static-functions" } */

static void bar () { }

/* { dg-final { scan-assembler "bar" } } */
