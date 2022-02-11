/* { dg-lto-do link } */
/* { dg-skip-if "power10 and above only" { ! { power10_ok } } } */
/* -Wno-attributes suppresses always_inline warnings.  */
/* { dg-lto-options { "-O2 -mdejagnu-cpu=power8 -flto -Wno-attributes -mno-power8-fusion" } } */

int __attribute__ ((always_inline))
foo1 (int *b)
{
  *b += 100;
  return *b;
}

