/* { dg-do compile } */
/* { dg-options "-O2 -Wmaybe-uninitialized -Werror"  } */

int cbos();
static int aos() {
  cbos();
  return 0;
}
int cbos_ptr;
long cbos_psize;
int cbos() {
  if (cbos_ptr)
    return aos();
  if (cbos_psize)
    return 1;
  return 0;
}
