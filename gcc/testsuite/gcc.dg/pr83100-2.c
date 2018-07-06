/* PR target/83100 */
/* { dg-do run } */
/* { dg-options "-O2 -fcommon -fdata-sections" } */
/* { dg-additional-sources pr83100-3.c } */
/* { dg-skip-if "-fdata-sections not supported" { hppa*-*-hpux* nvptx-*-* } } */

const int a;

int
main ()
{
  if (a != 7)
    __builtin_abort ();
  return 0;
}
