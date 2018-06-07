/* PR ipa/84722.  */
/* { dg-do run } */
/* { dg-require-ifunc "" } */

__attribute__ ((target_clones ("avx", "arch=core-avx2", "default"))) int
foo (int i)
{
  return i - 1;
}
int weaks (int i) __attribute__ ((weak, alias ("foo")));

int
main (int argc, char **argv)
{
  return weaks (argc);
}
