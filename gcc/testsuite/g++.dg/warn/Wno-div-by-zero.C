// { dg-options "-Wno-div-by-zero" }

int breakme()
{
  int x = 0;
  x /= 0;
  return x;
}
