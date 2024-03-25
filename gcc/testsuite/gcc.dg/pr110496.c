/* { dg-do compile } */
/* { dg-options "-O2" } */

long contents, f_num;
int decide();
int f_MV0__x;
void f() {
  unsigned char *rptr;
  unsigned char valbuf[6];
  rptr = (unsigned char *)contents;  /* { dg-warning "-Wint-to-pointer-cast" "" { target { ! ptr_eq_long } } } */
  if (decide())
    do {
      __builtin_memcpy(valbuf, &f_MV0__x, sizeof(int));
      (&valbuf[0])[4] = (&valbuf[0])[5] = 0;
    } while (0);
  else {
    int MV0__x = f_num;
    __builtin_memcpy(valbuf, &MV0__x, sizeof(int));
    (&valbuf[0])[4] = (&valbuf[0])[5] = 0;
  }
  rptr[1] = valbuf[4];
  rptr[2] = valbuf[5];
  rptr[4] = valbuf[1];
  rptr[5] = valbuf[2];
  __builtin_memset(valbuf, 0, 8);
}
