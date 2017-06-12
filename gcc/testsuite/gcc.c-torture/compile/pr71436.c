/* PR target/71436.  */

#pragma pack(1)
struct S0
{
  volatile int f0;
  short f2;
};

void foo (struct S0 *);
int a, d;
static struct S0 b[5];
static struct S0 c;
void fn1 ();
void
main ()
{
  {
    struct S0 e;
    for (; d; fn1 ())
      {
        {
          a = 3;
          for (; a >= 0; a -= 1)
            {
              {
                e = c;
              }
              b[a] = e;
            }
        }
      }
  }
  foo (b);
}
