/* { dg-do compile } */
/* { dg-skip-if "only works for ELF targets" { *-*-darwin* *-*-aix* } } */

__attribute__ ((__symver__ ("foo@VER_2")))
__attribute__ ((__symver__ ("foo@VER_3")))
int foo()
{
  return 2;
}

/* { dg-final { scan-assembler ".symver.*foo, _?foo@VER_2" } } */
/* { dg-final { scan-assembler ".symver.*foo, _?foo@VER_3" } } */
