/* { dg-do compile } */
/* { dg-options "-O2" } */
_Float16 in[128];
short out[128];
void foo(void) {
  for (int i = 0; i < 128; i++) {
    _Float16 x = in[i];
    _Float16 y = x ? -x : 0.0;
    short dst;
    __builtin_memcpy (&dst, &y, sizeof(dst));
    out[i] = dst;
  }
}
