/* { dg-require-effective-target indirect_calls } */

f(x)
{
  (*(void (*)())&x)();
}
