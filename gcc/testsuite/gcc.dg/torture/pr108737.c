/* { dg-do run } */

extern void exit (int);
extern void abort (void);

void __attribute__((noipa)) foo () { exit (0); }

void __attribute__((noipa)) blah (int x)
{
  while (1) {
      if(x) foo();
  }
}

int main()
{
  blah (1);
  abort ();
}
