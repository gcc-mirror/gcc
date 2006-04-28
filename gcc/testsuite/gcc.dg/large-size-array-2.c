/* PR c/25309 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
static char * name[] = {
    [0x80000000]  = "bar"
  };

