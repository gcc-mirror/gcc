/* { dg-do run } */
/* { dg-options "-O2" } */

/* VRP was miscompiling the following as it thought &a->b was a dereference
   and therfore a was non-null.  
   Reduced from Mozilla by Serge Belyshev <belyshev@depni.sinp.msu.ru>.  */

extern "C" void abort (void);
struct T { int i; } t;
struct A : T { int j; } *p = __null;

int main (void)
{
  if (p == &t)
    return 0;
  if (p)
    abort ();
  return 0;
}

