// { dg-do run }
// { dg-options "-O3" }

int printf(const char *, ...);
short a;
long b = 3, c;
int d(int e) {
  switch (e)
  case 111:
  case 222:
  case 44:
    return 0;
  return e;
}
int main() {
  for (; a >= 0; --a)
    if (d(c + 23) - 23)
      b = 0;

  if (b != 3)
    __builtin_abort ();
}
