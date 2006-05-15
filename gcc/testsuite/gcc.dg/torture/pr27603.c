/* { dg-do run } */

void exit (int);
void abort (void);
int a;
int main()
{
  int j;
  for (j = 0; j < 6; j++)
  {
    if ((unsigned)j - 3 <= 1)
      exit (0);
    a = 1000 * (6 - j);
  }
  abort ();
}
