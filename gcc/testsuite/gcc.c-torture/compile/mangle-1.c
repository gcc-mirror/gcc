
#if __nvptx__
/* Doesn't like . in labels.  */
#define SEP "$"
#else
#define SEP "."
#endif

int foo(void)
{
  static int x asm ("x") = 3;
  return x++;
}

int X2 asm ("x" SEP "0") = 4;
int X3 asm ("_x" SEP "0") = 5;

