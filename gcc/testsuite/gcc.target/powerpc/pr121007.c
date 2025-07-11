/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power9" } */

typedef struct { int a; } A;
unsigned char *a;
char b;
int c;
void foo (vector char, vector char, vector char);

void
bar (long stride)
{
  vector char v0, v1, v2, v3, v5;
  vector char r0 = __builtin_vec_vsx_ld (0, a);
  vector char r2 = __builtin_vec_vsx_ld (2 * stride, a - 3);
  vector char r3 = __builtin_vec_vsx_ld (3 * stride, a - 3);
  vector char r4;
  vector char r6 = __builtin_vec_vsx_ld (6 * stride, a - 3);
  vector char r7 = __builtin_vec_vsx_ld (7 * stride, a - 3);
  vector char r14, h, i, j;
  if (b)
    return;
  v1 = __builtin_vec_vsx_ld (9 * stride, a);
  v2 = __builtin_vec_vsx_ld (10 * stride, a - 3);
  v3 = __builtin_vec_vsx_ld (11 * stride, a - 3);
  r3 = __builtin_vec_mergeh (r3, v3);
  v5 = __builtin_vec_mergel (r2, r6);
  r14 = __builtin_vec_mergeh (r3, r7);
  r4 = __builtin_vec_mergeh (v2, r14);
  v0 = __builtin_vec_mergeh (r0, r4);
  union { unsigned char a[16]; A b; } temp;
  vector signed char k;
  h = __builtin_vec_ld (0, temp.a);
  i = __builtin_vec_splat (h, 1);
  temp.b.a = c;
  k = __builtin_vec_ld (0, (signed char *) temp.a);
  j = __builtin_vec_and (i, (vector char) k);
  foo (v1, v0, j);
  foo (v1, v5, j);
}
