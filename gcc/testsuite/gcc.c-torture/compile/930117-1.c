/* { dg-require-effective-target indirect_calls } */
/* { dg-additional-options "-std=gnu89" } */

f(x)
{
  (*(void (*)())&x)();
}
