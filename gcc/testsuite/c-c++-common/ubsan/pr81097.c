/* { dg-do run } */
/* { dg-options "-fsanitize=undefined -fsanitize-undefined-trap-on-error" } */

unsigned int a = 3309568;
unsigned int b = -1204857327;
short c = -10871;
short x;
int main()
{
  x = ((short)(~a) | ~c) +  ((short)(~b) | ~c);
  return 0;
}
