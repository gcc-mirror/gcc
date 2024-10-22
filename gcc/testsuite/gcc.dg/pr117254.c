/* { dg-do compile } */
/* { dg-options "" } */

int g;
void e(int s) {
  struct {
    __attribute__((nonstring)) char bn[g];
  } f;
  __builtin_strncpy (f.bn, f.bn, s);
}
