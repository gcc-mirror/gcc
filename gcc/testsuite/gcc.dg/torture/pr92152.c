/* { dg-do run } */
union U
{
  long long i;
  long f;
} v;
 	
long
foo (long *f)
{
  *f = 1;
  v.i = 0;
  v.f = 0;
  return *f;
}
 	
int
main ()
{
  if (foo (&v.f) != 0)
    __builtin_abort ();
  return 0;
}
