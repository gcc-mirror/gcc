// PR c++/13371
// Bug: We were failing to properly protect the lhs on the line marked
// "here" from multiple evaluation.

// { dg-do run }

extern "C" int printf (const char *, ...);

enum E { E1, E2 };

struct A
{
  E e : 8;
  unsigned char c;
};

A ar[2];

int c;

int f()
{
  ++c;
  printf ("f()\n");
  return 0;
}

int main()
{
  ar[0].c = 0xff;
  ar[f()].e = E1;		// here
  return (c != 1 || ar[0].c != 0xff);
}  
