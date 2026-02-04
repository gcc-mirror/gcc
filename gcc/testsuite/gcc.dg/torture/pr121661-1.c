/* { dg-do run } */
/* { dg-options "" } */
/* PR middle-end/121661 */

static void fun1(_Complex int val)
{
  __attribute__((unused))
  void nfun()
    {
      if (__real__ val != 1)
        __builtin_abort();
    }
  (void)&val;
  if (__real__ val != 1)
    __builtin_abort();
}

int main(int argc, char* [])
{
  fun1(1);
}
