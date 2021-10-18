/* Test __attribute__ ((unavailable)) */
/* { dg-do compile } */
/* { dg-options "" } */

int g_nn;
int& g_n __attribute__((unavailable)) = g_nn;

void f()
{
  int f_nn;
  int& f_n __attribute__((unavailable)) = f_nn;
  f_n = 1;    // { dg-error "'f_n' is unavailable" }
}

int main()
{
  g_n = 1;    // { dg-error "'g_n' is unavailable" }
  f();
}
