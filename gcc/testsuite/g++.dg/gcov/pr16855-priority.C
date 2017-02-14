/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */
/* { dg-require-effective-target init_priority } */

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
  ~Test (void) { fprintf (stderr, "In Test::~Test\n"); /* count(1) */ }
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

static void __attribute__ ((constructor ((101)))) ctor_100 ()
{
  fprintf (stderr, "in constructor((101))\n"); /* count(1) */
}

static void __attribute__ ((constructor ((500)))) ctor_500 ()
{
  fprintf (stderr, "in constructor((500))\n"); /* count(1) */
}

static void __attribute__ ((constructor ((65535)))) ctor_65535 ()
{
  fprintf (stderr, "in constructor((65535))\n"); /* count(1) */
}

static void __attribute__ ((destructor)) dtor_default ()
{
  fprintf (stderr, "in destructor(())\n"); /* count(1) */
}

static void __attribute__ ((destructor ((101)))) dtor_100 ()
{
  fprintf (stderr, "in destructor((101))\n"); /* count(1) */
}

static void __attribute__ ((destructor ((500)))) dtor_500 ()
{
  fprintf (stderr, "in destructor((500))\n"); /* count(1) */
}

static void __attribute__ ((destructor ((65535)))) dtor_65535 ()
{
  fprintf (stderr, "in destructor((65535))\n"); /* count(1) */
}

/* { dg-final { run-gcov branches { -b pr16855-priority.C } } } */
