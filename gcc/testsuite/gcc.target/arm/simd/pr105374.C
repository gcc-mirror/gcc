/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */

typedef float v4f __attribute__((vector_size(4 * sizeof(float))));
v4f f_x, f_y;
long f() { return (f_x < f_y | f_x <= f_y)[2]; }
