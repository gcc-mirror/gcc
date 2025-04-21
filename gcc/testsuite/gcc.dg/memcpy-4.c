/* { dg-do compile } */
/* { dg-options "-O2" } */

void
f1 (char *p)
{
  __builtin_memcpy (p, "12345", 5);
}
