/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

extern void abort (void);
unsigned short __attribute__((noinline))
foo (int i)
{
  if (i >= 0
      && i <= 0x400000)
    return (unsigned short)(signed char)i;
  return i;
}
int main()
{
  int i;
  for (i = 0; i < 0xffff; ++i)
    if (foo(i) != (unsigned short)(signed char) i)
      abort ();
  return 0;
}
