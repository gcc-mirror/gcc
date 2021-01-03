/* { dg-do run { target { power10_hw } } } */
/* { dg-do link { target { ! power10_hw } } } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Verify that we generate an indirect sibcall for ELFv2 on P10 and
   later, with r12 and CTR containing the function address.  PR96787.  */

extern void abort (void);
extern int f (int);

int main ()
{
  if (f (3) != 6)
    abort ();
  return 0;
}


int g (int a)
{
  return a * 2;
}


int h (int a)
{
  return a + 2;
}

int __attribute__((__noinline__)) f (int a)
{
  int (*x) (int) = a % 2 ? &g : &h;
  (*x) (a);
}

