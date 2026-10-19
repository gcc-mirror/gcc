/* { dg-lto-do ar-link } */
/* { dg-lto-options { { -flto=auto -ffat-lto-objects } } } */
/* { dg-skip-if "Target ar tool experiences pipe buffer deadlocks on multi-object LTO streams" { hppa*-*-hpux* } } */

extern int bar_7 (int);

int main (void)
{
  return bar_7 (42);
}
