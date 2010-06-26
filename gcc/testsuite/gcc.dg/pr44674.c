/* { dg-do compile } */
/* { dg-options "-O -fprofile-generate" } */

void
jumpfunc (void *p)
{
  void *l = &&jumplabel;
jumplabel:
  __builtin_memcpy (p, l, 1);
}
