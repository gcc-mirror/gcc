/* { dg-do run } */

float global;
int main()
{
  unsigned long z = 1;
  float x = -z;
  global = x;
  if (global < 0)
    __builtin_abort ();
  return 0;
}
