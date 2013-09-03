/* { dg-do run } */

extern void abort (void);

int a, b; 

int main ()
{
  int t[2] = {1,1};

  for (a = 0; a < 2; a++)
    {
      b ^= t[a];
      t[a] = t[1] = 0;
    }

  if (b != 1)
    abort ();

  return 0;
}
