/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

extern void abort (void);

char a = 0, b = 0, c = 0, d = 0;

int main()
{
  a = -(b - 405418259) - ((d && c) ^ 2040097152);
  if (a != -109)
    abort();
  return 0;
}
