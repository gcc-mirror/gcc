/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target tls } */

__thread struct
{
  int a;
  char b[32];
} thr;

int
main ()
{
  __builtin_strcpy (thr.b, "abcd");
  return 0;
}
