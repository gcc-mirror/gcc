/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

int a;
void b()
{
  long c = 3;
  unsigned int d = 50253292;
  int e = 2147483648;
  for (; a < 5; a++)
    do {
      e += 4;
      d -= c;
    } while (e < 20);
  if (d != -1560359471u)
    __builtin_abort ();
}
int main() { b(); }
