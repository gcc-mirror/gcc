/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13" } */

/* { dg-final { scan-assembler-times "veslb" 2 } } */
/* { dg-final { scan-assembler-times "veslh" 2 } } */
/* { dg-final { scan-assembler-times "veslf" 2 } } */
/* { dg-final { scan-assembler-times "veslg" 2 } } */

/* { dg-final { scan-assembler-times "vesrab" 1 } } */
/* { dg-final { scan-assembler-times "vesrah" 1 } } */
/* { dg-final { scan-assembler-times "vesraf" 1 } } */
/* { dg-final { scan-assembler-times "vesrag" 1 } } */

/* { dg-final { scan-assembler-times "vesrlb" 1 } } */
/* { dg-final { scan-assembler-times "vesrlh" 1 } } */
/* { dg-final { scan-assembler-times "vesrlf" 1 } } */
/* { dg-final { scan-assembler-times "vesrlg" 1 } } */

/* { dg-final { scan-assembler-times "veslvb" 2 } } */
/* { dg-final { scan-assembler-times "veslvh" 2 } } */
/* { dg-final { scan-assembler-times "veslvf" 2 } } */
/* { dg-final { scan-assembler-times "veslvg" 2 } } */

/* { dg-final { scan-assembler-times "vesravb" 1 } } */
/* { dg-final { scan-assembler-times "vesravh" 1 } } */
/* { dg-final { scan-assembler-times "vesravf" 1 } } */
/* { dg-final { scan-assembler-times "vesravg" 1 } } */

/* { dg-final { scan-assembler-times "vesrlvb" 1 } } */
/* { dg-final { scan-assembler-times "vesrlvh" 1 } } */
/* { dg-final { scan-assembler-times "vesrlvf" 1 } } */
/* { dg-final { scan-assembler-times "vesrlvg" 1 } } */

typedef __attribute__((vector_size(16))) signed char v16qi;
typedef __attribute__((vector_size(16))) unsigned char uv16qi;

typedef __attribute__((vector_size(16))) signed short v8hi;
typedef __attribute__((vector_size(16))) unsigned short uv8hi;

typedef __attribute__((vector_size(16))) signed int v4si;
typedef __attribute__((vector_size(16))) unsigned int uv4si;

typedef __attribute__((vector_size(16))) signed long long v2di;
typedef __attribute__((vector_size(16))) unsigned long long uv2di;

uv16qi g_uvqi0, g_uvqi1, g_uvqi2;
v16qi g_vqi0, g_vqi1, g_vqi2;

uv8hi g_uvhi0, g_uvhi1, g_uvhi2;
v8hi g_vhi0, g_vhi1, g_vhi2;

uv4si g_uvsi0, g_uvsi1, g_uvsi2;
v4si g_vsi0, g_vsi1, g_vsi2;

uv2di g_uvdi0, g_uvdi1, g_uvdi2;
v2di g_vdi0, g_vdi1, g_vdi2;

void
shift_left_by_scalar (int s)
{
  g_uvqi0 = g_uvqi1 << s;
  g_vqi0 = g_vqi1 << s;
  g_uvhi0 = g_uvhi1 << s;
  g_vhi0 = g_vhi1 << s;
  g_uvsi0 = g_uvsi1 << s;
  g_vsi0 = g_vsi1 << s;
  g_uvdi0 = g_uvdi1 << s;
  g_vdi0 = g_vdi1 << s;
}

void
shift_right_by_scalar (int s)
{
  g_uvqi0 = g_uvqi1 >> s;
  g_vqi0 = g_vqi1 >> s;
  g_uvhi0 = g_uvhi1 >> s;
  g_vhi0 = g_vhi1 >> s;
  g_uvsi0 = g_uvsi1 >> s;
  g_vsi0 = g_vsi1 >> s;
  g_uvdi0 = g_uvdi1 >> s;
  g_vdi0 = g_vdi1 >> s;
}

void
shift_left_by_vector ()
{
  g_uvqi0 = g_uvqi1 << g_uvqi2;
  g_vqi0 = g_vqi1 << g_vqi2;
  g_uvhi0 = g_uvhi1 << g_uvhi2;
  g_vhi0 = g_vhi1 << g_vhi2;
  g_uvsi0 = g_uvsi1 << g_uvsi2;
  g_vsi0 = g_vsi1 << g_vsi2;
  g_uvdi0 = g_uvdi1 << g_uvdi2;
  g_vdi0 = g_vdi1 << g_vdi2;
}

void
shift_right_by_vector ()
{
  g_uvqi0 = g_uvqi1 >> g_uvqi2;
  g_vqi0 = g_vqi1 >> g_vqi2;
  g_uvhi0 = g_uvhi1 >> g_uvhi2;
  g_vhi0 = g_vhi1 >> g_vhi2;
  g_uvsi0 = g_uvsi1 >> g_uvsi2;
  g_vsi0 = g_vsi1 >> g_vsi2;
  g_uvdi0 = g_uvdi1 >> g_uvdi2;
  g_vdi0 = g_vdi1 >> g_vdi2;
}
