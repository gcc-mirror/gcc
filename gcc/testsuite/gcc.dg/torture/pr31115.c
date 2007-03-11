/* { dg-do run } */

extern void exit(int);
extern void abort();
void foo (int e1)
{
  if (e1 < 0)
    {
      e1 = -e1;
      if (e1 >>= 4)
        {
          if (e1 >= 1 << 5)
            exit(0);
        }
    }
}

int main()
{
  foo(-(1<<9));
  abort();
}
