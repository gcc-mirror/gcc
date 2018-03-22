// { dg-do compile }
// { dg-require-effective-target alloca }

int foo()
{
  int a = 1;
  int b = 1;
  int e[a][b];
  e[0][0] = 0;
  return e[a-1][b-1];
}
