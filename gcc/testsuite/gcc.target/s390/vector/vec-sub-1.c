/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13" } */

/* { dg-final { scan-assembler-times "vsb" 2 } } */
/* { dg-final { scan-assembler-times "vsh" 2 } } */
/* { dg-final { scan-assembler-times "vsf" 2 } } */
/* { dg-final { scan-assembler-times "vsg" 2 } } */
/* { dg-final { scan-assembler-times "vfs" 1 } } */


typedef unsigned char     uv16qi __attribute__((vector_size(16)));
typedef signed char        v16qi __attribute__((vector_size(16)));
typedef unsigned short     uv8hi __attribute__((vector_size(16)));
typedef signed short        v8hi __attribute__((vector_size(16)));
typedef unsigned int       uv4si __attribute__((vector_size(16)));
typedef signed int          v4si __attribute__((vector_size(16)));
typedef unsigned long long uv2di __attribute__((vector_size(16)));
typedef signed long long    v2di __attribute__((vector_size(16)));
typedef double              v2df __attribute__((vector_size(16)));

uv16qi g_uvqi0, g_uvqi1, g_uvqi2;
v16qi g_vqi0, g_vqi1, g_vqi2;

uv8hi g_uvhi0, g_uvhi1, g_uvhi2;
v8hi g_vhi0, g_vhi1, g_vhi2;

uv4si g_uvsi0, g_uvsi1, g_uvsi2;
v4si g_vsi0, g_vsi1, g_vsi2;

uv2di g_uvdi0, g_uvdi1, g_uvdi2;
v2di g_vdi0, g_vdi1, g_vdi2;

v2df g_vdf0, g_vdf1, g_vdf2;

void
sub1 ()
{
  g_vqi0 = g_vqi1 - g_vqi2;
  g_uvqi0 = g_uvqi1 - g_uvqi2;

  g_vhi0 = g_vhi1 - g_vhi2;
  g_uvhi0 = g_uvhi1 - g_uvhi2;

  g_vsi0 = g_vsi1 - g_vsi2;
  g_uvsi0 = g_uvsi1 - g_uvsi2;

  g_vdi0 = g_vdi1 - g_vdi2;
  g_uvdi0 = g_uvdi1 - g_uvdi2;

  g_vdf0 = g_vdf1 - g_vdf2;
}
