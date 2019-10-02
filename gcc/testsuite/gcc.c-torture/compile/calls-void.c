/* { dg-require-effective-target ptr32plus } */
/* { dg-require-effective-target untyped_assembly } */
/* { dg-require-effective-target indirect_calls } */

typedef void  (*T)(void);
f1 ()
{
  ((T) 0)();
}
f2 ()
{
  ((T) 1000)();
}
f3 ()
{
  ((T) 10000000)();
}
f4 (r)
{
  ((T) r)();
}
f5 ()
{
  int (*r)() = f3;
  ((T) r)();
}
