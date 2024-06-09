/* { dg-require-effective-target trampolines } */
/* { dg-additional-options "-std=gnu89" } */

int f1()
{
  { int ___() { foo(1); } bar(___); }
  return( { int ___() { foo(2); } bar(___);} );
}

int f2(int j)
{
  { int ___() { foo(j); } bar(___); }
  return( { int ___() { foo(j); } bar(___);} );
}
