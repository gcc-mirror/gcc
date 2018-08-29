/* { dg-do run } */
/* { dg-options "-fsanitize=undefined -fsanitize-undefined-trap-on-error" } */

short s = 2;
short y = 1;
int i;
int main()
{
  i = -(s + (int)(~(unsigned)(0 / y))) + 0x7fffffff;
  return 0;
}
