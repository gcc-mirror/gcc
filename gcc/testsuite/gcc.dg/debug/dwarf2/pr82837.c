/* PR debug/82837 */
/* { dg-do compile } */
/* { dg-options "-O2 -g" } */
/* { dg-additional-options "-march=athlon" { target ia32 } } */
/* { dg-additional-options "-fPIE" { target pie } } */

static char b[100];
static int *c;
char *e;
void a(char *f, char *i) {
  int d = __builtin_object_size(f, 1);
  __builtin___strcpy_chk(f, i, d);
}
void g(void) {
  int h;
  switch (*c) {
  case 8:
    e = "swapgs";
    break;
  case 9:
    e = "rdtscp";
    break;
  default:
    return;
  }
  h = __builtin_strlen(b);
  a(b + h - 6, e);
  c++;
}
