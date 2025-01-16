// PR c++/118214
// { dg-do run { target c++11 } }
// { dg-options "" }

struct A { int a[256]; };
unsigned char b[] = {
#embed __FILE__ limit (160)
};

void
foo (A a)
{
  for (int i = 0; i < 256; ++i)
    if (a.a[i] != (i < sizeof (b) ? b[i] : 0))
      __builtin_abort ();
}

int
main ()
{
  foo ({
#embed __FILE__ limit (160)
       });
}
