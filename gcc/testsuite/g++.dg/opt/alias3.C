// { dg-options "-O2" }

// Contributed by Nathan Sidwell 22 Dec 2003 <nathan@codesourcery.com>
// Origin: rsandifo@redhat.com

// PR c++/13387. Alias sets were incorrect

struct C {
  C(short *p = 0, int i = 0) : ptr (p), index (i) {}
  short operator*() { return ptr[index]; }
  short *ptr;
  int index;
};

C f1 (C) __attribute__ ((noinline));
C f1 (C x)
{
  return x;
}

void f2 (short)__attribute__ ((noinline));;
short s;

void f2 (short s_)
{
  s = s_;
}

C g (C x)__attribute__ ((noinline));
C g (C x)
{
  x = f1 (x); 
  f2 (*x);
  return x;
}

int main ()
{
  short p[2] = { 0x1234, 0x5678 };
  C x (p, 1);

  g (x);

  return s != p[1];
}
