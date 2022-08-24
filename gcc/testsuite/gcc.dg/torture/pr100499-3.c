/* { dg-do run } */

int a = 0;
unsigned char b = 0;

int main() {
  a - 6;
  for (; a >= -13; a = a - 8)
    while((unsigned char)(b-- * 6))
      ;
  if (b != 127)
    __builtin_abort();
  return 0;
}
