/* The return argument needs a type conversion which on some targets
   (e.g. s390) needs additional code.  So it is invalid to do tail
   call optimization here.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */

extern void abort (void);

long long __attribute__((noinline))
foo ()
{
  return 3;
}

int __attribute__((noinline))
boo ()
{
  return foo ();
}

int
main ()
{
  if (boo () != 3)
    abort ();
}

