const int a = 3;
const int b = 50;

void foo (void)
{
  long int x[a][b];
  asm ("" : : "r" (x) : "memory");
}
