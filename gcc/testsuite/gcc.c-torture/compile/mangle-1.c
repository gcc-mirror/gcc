int foo(void)
{
  static int x asm ("x") = 3;
  return x++;
}

int X2 asm ("x.0") = 4;
int X3 asm ("_x.0") = 5;

