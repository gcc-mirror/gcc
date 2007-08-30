/* Contributed by Joost VandeVondele  <jv244@cam.ac.uk> */

/* { dg-do run } */
/* { dg-require-profiling "-pg" } */
/* { dg-options "-O2 -pg" } */
/* { dg-options "-O2 -pg -mtune=core2" { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-O2 -pg -static" { target hppa*-*-hpux* } } */

extern void abort (void);

int stack_pointer;

void
__attribute__((noinline))
mystop ()
{
  abort ();
}

void
__attribute__((noinline))
add ()
{
  if (stack_pointer + 1 > 10)
    mystop ();

  stack_pointer = stack_pointer + 1;
}

int main ()
{
  add ();
  return stack_pointer - 1;
}

/* { dg-final { cleanup-profile-file } } */
