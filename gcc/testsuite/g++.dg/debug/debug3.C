// PR optimization/5547
// This testcase caused ICE on IA-32, since DWARF-2 was unable
// to emit location expression for parameter a of operator+.
// { dg-do compile }
// { dg-options "-fpic" }
// { dg-warning "not supported" "PIC unsupported" { target cris-*-elf* cris-*-aout* mmix-*-* } 0 }

struct A { char *s; };

inline A operator+ (char a, const A &b)
{
  A s;
  s.s = new char[12];
  s.s[0] = a;
  return s;
}

int b (const A &);

void test1 (const A &x, int y)
{
  int j = b ("012345"[y] + x);
  for (int i = 0; i < y; i++);
}

void test2 (const A &x, int y)
{
  int j = b ("012345678"[y + 2] + x);
  for (int i = 0; i < y; i++);
}

void test3 (const A &x, int y)
{
  int j = b ("012345678"[y - 6] + x);
  for (int i = 0; i < y; i++);
}

void test4 (const A &x, int y)
{
  int j = b ("012345678"[2 * y - 10] + x);
  for (int i = 0; i < y; i++);
}

void test5 (const A &x, int y)
{
  int j = b ("012345678"[4 * y] + x);
  for (int i = 0; i < y; i++);
}
