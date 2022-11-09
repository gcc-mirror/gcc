// { dg-do run }
// { dg-options "-O1" }

unsigned char a = 1;
char b, e;
long long c;
short d;
int main() {
  if (sizeof (short) != 2 || sizeof (int) != 4 || sizeof (long long) != 8)
    return 0;
  a = ~(1 && a);
  c = ~((~a / 8 | -2) & 11007578330939886389LLU);
  e = -c;
  d = ~c / e;
  if (d < 2000)
    __builtin_abort();
  return 0;
}
