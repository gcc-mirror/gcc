/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

/* See PR91087 for information on Darwin xfails. */

#include <stdlib.h>
#include <stdio.h>

int a;

void
foo ()
{
  fprintf (stderr, "In foo\n");
  a = 123; /* count(1) */
}

using namespace std;
class Test
{
public:
  Test (void) { fprintf (stderr, "In Test::Test\n"); /* count(1) */ }
  ~Test (void) {
   fprintf (stderr, "In Test::~Test\n"); /* count(1) { xfail *-*-darwin* *-*-dragonfly* } */
  }
} T1;

void
uncalled (void)
{
  fprintf (stderr, "In uncalled\n"); /* count(#####) */
}

int
main (void)
{
  atexit (&foo);
  fprintf (stderr, "In main\n"); /* count(1) */
  return 0;
}

static void __attribute__ ((constructor)) ctor_default ()
{
  fprintf (stderr, "in constructor(())\n"); /* count(1) */
}

static void __attribute__ ((destructor)) dtor_default ()
{
  fprintf (stderr, "in destructor(())\n"); /* count(1) { xfail *-*-darwin* } */
}

/* { dg-final { run-gcov branches { -b pr16855.C } { xfail *-*-darwin* *-*-dragonfly* } } } */
