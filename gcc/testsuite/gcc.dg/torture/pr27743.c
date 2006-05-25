/* { dg-do run } */

extern void abort(void);

int bar(int a)
{
  return ((unsigned) ((a) >> 2)) >> 15;
}

int main()
{
  if (bar (0xffff3000) != 0x1ffff)
    abort ();
  return 0;
}
