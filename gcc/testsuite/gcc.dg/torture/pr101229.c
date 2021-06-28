/* { dg-do compile } */

int a[1024];
void foo()
{
  for (int i; i; i += 4) {
    int suma = a[i];
    int sumb = a[i + 1];
    int sumc;
    for (unsigned j = 0; j < 77; ++j) {
      suma = (suma ^ i) + 1;
      sumb = (sumb ^ i) + 2;
      sumc = suma ^ i;
    }
    a[i] = suma;
    a[i + 1] = sumb;
    a[i + 2] = sumc;
  }
}
