// { dg-do run }
// { dg-options "-O1" }

unsigned char a = 1;
char b, e;
long c;
short d;
int main() {
  a = ~(1 && a);
  c = ~((~a / 8 | -2) & 11007578330939886389LLU);
  e = -c;
  d = ~c / e;
  if (d < 2000)
    __builtin_abort();
  return 0;
}
