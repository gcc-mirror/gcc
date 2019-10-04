// { dg-additional-options "-Wformat" }

extern "C" int printf (const char *, ...);

struct X {
  unsigned long long a: 1;
} x;

void foo()
{
  printf("%d", x.a);
}
