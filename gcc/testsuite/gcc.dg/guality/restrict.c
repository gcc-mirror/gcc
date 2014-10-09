/* debuginfo tests for combinations of const, volatile, restrict pointers. */
/* { dg-do run } */
/* { dg-options "-std=c99 -gdwarf-3" } */

int *ip __attribute__((used));
const int *cip __attribute__((used));
int * restrict irp __attribute__((used));
int * const icp __attribute__((used));
const int * restrict cirp __attribute__((used));
int * const restrict icrp __attribute__((used));
const int * const restrict cicrp __attribute__((used));

int * const volatile restrict cvirp __attribute__((used));
const volatile int * restrict pcvir __attribute__((used));

static __attribute__((noclone, noinline)) void *
cpy (void * restrict s1, const void * restrict s2, unsigned int n)
{
  char *t1 = s1;
  const char *t2 = s2;
  while(n-- > 0)
    *t1++ = *t2++;
  return s1;
}

int
main (int argc, char **argv)
{
  void *foo = 0;
  if (argc > 16)
    foo = cpy (argv[0], argv[1], argc);

  return foo != 0;
}

/* { dg-final { gdb-test 30 "type:ip" "int *" } } */
/* { dg-final { gdb-test 30 "type:cip" "const int *" } } */
/* { dg-final { gdb-test 30 "type:irp" "int * restrict" } } */
/* { dg-final { gdb-test 30 "type:icp" "int * const" } } */
/* { dg-final { gdb-test 30 "type:cirp" "const int * restrict" } } */
/* { dg-final { gdb-test 30 "type:icrp" "int * const restrict" } } */
/* { dg-final { gdb-test 30 "type:cicrp" "const int * const restrict" } } */

/* { dg-final { gdb-test 30 "type:cvirp" "int * const volatile restrict" } } */
/* { dg-final { gdb-test 30 "type:pcvir" "const volatile int * restrict" } } */

/* { dg-final { gdb-test 30 "type:main" "int (int, char **)" } } */
/* { dg-final { gdb-test 30 "type:cpy" "void *(void * restrict, const void * restrict, unsigned int)" } } */
