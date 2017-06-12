/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target alloca } */

void foo();

void func()
{
  int m;

  int tab[m];

  __int128 j;
  for(; j; j++)
    {
      tab[j] = 0;
      tab[j+1] = 0;
    }

  foo();
}
