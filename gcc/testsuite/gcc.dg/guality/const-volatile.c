/* debuginfo tests for combinations of const and volatile type qualifiers. */
/* { dg-do run } */
/* { dg-options "-g" } */

int i __attribute__((used));
const int ci __attribute__((used));
volatile int vi;
const volatile int cvi;

int *pi __attribute__((used));
const int *pci __attribute__((used));
volatile int *pvi __attribute__((used));
const volatile int *pcvi __attribute__((used));

int * const cip __attribute__((used));
int * volatile vip;
int * const volatile cvip;

volatile struct
{
  const long cli;
  const signed char csc;
} vs;

struct foo
{
  const long cli;
  const signed char csc;
};

struct bar
{
  short s;
  const short cs;
  volatile short vs;
  const volatile short cvs;
  volatile long long vll;
};

struct bar bar __attribute__((used));
struct foo foo __attribute__((used));
const struct foo cfoo __attribute__((used));
volatile struct foo vfoo;
const volatile struct foo cvfoo;

typedef volatile signed char score;

score s;
const score cs;

static __attribute__((noclone, noinline)) int
f (const char *progname, volatile struct foo *dummy, const score s)
{
  return progname == 0 || dummy == 0 || dummy->csc == s;
}

int
main (int argc, char **argv)
{
  score as = argc;
  struct foo dummy = { 1, 1 };
  return f (argv[0], &dummy, as) - 1;
}

/* { dg-final { gdb-test 50 "type:main" "int (int, char **)" } } */

/* { dg-final { gdb-test 50 "type:i" "int" } } */
/* { dg-final { gdb-test 50 "type:ci" "const int" } } */
/* { dg-final { gdb-test 50 "type:vi" "volatile int" } } */
/* { dg-final { gdb-test 50 "type:cvi" "const volatile int" } } */

/* { dg-final { gdb-test 50 "type:pi" "int *" } } */
/* { dg-final { gdb-test 50 "type:pci" "const int *" } } */
/* { dg-final { gdb-test 50 "type:pvi" "volatile int *" } } */
/* { dg-final { gdb-test 50 "type:pcvi" "const volatile int *" } } */

/* { dg-final { gdb-test 50 "type:cip" "int * const" } } */
/* { dg-final { gdb-test 50 "type:vip" "int * volatile" } } */
/* { dg-final { gdb-test 50 "type:cvip" "int * const volatile" } } */

/* { dg-final { gdb-test 50 "type:vs" "volatile struct { const long cli; const signed char csc; }" } } */

/* { dg-final { gdb-test 50 "type:cvip" "int * const volatile" } } */

/* { dg-final { gdb-test 50 "type:bar" "struct bar { short s; const short cs; volatile short vs; const volatile short cvs; volatile long long vll; }" } } */
/* { dg-final { gdb-test 50 "type:foo" "struct foo { const long cli; const signed char csc; }" } } */
/* { dg-final { gdb-test 50 "type:cfoo" "const struct foo { const long cli; const signed char csc; }" } } */
/* { dg-final { gdb-test 50 "type:vfoo" "volatile struct foo { const long cli; const signed char csc; }" } } */
/* { dg-final { gdb-test 50 "type:cvfoo" "const volatile struct foo { const long cli; const signed char csc; }" } } */

/* { dg-final { gdb-test 58 "type:s" "volatile signed char" } } */
/* { dg-final { gdb-test 50 "type:cs" "const volatile signed char" } } */

/* { dg-final { gdb-test 50 "type:f" "int (const char *, volatile struct foo *, const score)" } } */
