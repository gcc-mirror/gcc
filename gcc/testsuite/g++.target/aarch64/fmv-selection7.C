/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O2 -march=armv8-a" } */

[[gnu::optimize("O0")]]
[[gnu::target_version ("default")]]
int bar () {
  return 1;
}

[[gnu::optimize("O0")]]
[[gnu::target ("+sve2")]]
[[gnu::target_version ("sve")]]
int bar ();

[[gnu::target ("+sve")]]
int foo () {
  return bar();
}

/* { dg-final { scan-assembler-times "\n\tb\t_Z3barv\._Msve\n" 1 } } */

[[gnu::target_version ("default")]]
int bar2 () {
  return 1;
}

[[gnu::target_version ("sve2")]]
int bar2 ();

[[gnu::target_version ("default")]]
int foo2 ();

[[gnu::target_version ("sve")]]
[[gnu::target ("+sve2")]]
int foo2 () {
  return bar2();
}

/* { dg-final { scan-assembler-times "\n\tb\t_Z4bar2v\._Msve2\n" 1 } } */

[[gnu::target_version ("default")]]
int bar3 () {
  return 1;
}

[[gnu::target_version ("sve")]]
int bar3 ();

[[gnu::target ("+rng")]]
[[gnu::target_version ("sve2")]]
int bar3 ();

[[gnu::target_version ("default")]]
int foo3 ();

[[gnu::target_version ("sve")]]
int foo3 () {
  return bar3 ();
}

[[gnu::target_version ("sve2+rng")]]
int foo3 ();

/* { dg-final { scan-assembler-times "\n\tb\t_Z4bar3v\n" 1 } } */
