/* { dg-lto-do ar-link } */
/* { dg-lto-options { { -flto=auto -ffat-lto-objects } } } */

extern int bar_7 (int);

int main (void)
{
  return bar_7 (42);
}
