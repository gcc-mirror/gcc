/* { dg-do compile } */
/* { dg-options "-Wunused" } */

int foo ()
{
  asm goto ("" : : : : label);
  return 1;
 label:
  return 0;
}
