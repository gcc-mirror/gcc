/* { dg-additional-options "-std=gnu89" } */

f()
{
  long l2;
  unsigned short us;
  unsigned long ul;
  short s2;

  ul = us = l2 = s2 = -1;
  return ul;
}

main()
{
  if (f()!=(unsigned short)-1)
    abort();
  exit(0);
}
