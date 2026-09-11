/* { dg-do compile } */
/* { dg-options "-O3 -mrvv-max-lmul=conv-dynamic -march=rv64gcv -mabi=lp64d -mtune=generic-ooo" } */

typedef double VECTOR[3];
enum { X, Y, Z };
struct Bicubic_Patch_Struct {
  VECTOR Control_Points[4][4];
  VECTOR Bounding_Sphere_Center;
  double Bounding_Sphere_Radius;
};
void Assign_Vector(VECTOR d, VECTOR s) {
  d[X] = s[X];
  d[Y] = s[Y];
  d[Z] = s[Z];
}
typedef struct Bicubic_Patch_Struct BICUBIC_PATCH;
namespace std {
void find_average(int vector_count, VECTOR *vectors, VECTOR, double *radius) {
  int i;
  double r0, r1, yc, zc, y0;
  for (i = 0; i < vector_count; i++) {
    yc += vectors[i][Y];
    zc += vectors[i][Z];
  }
  y0 = yc - zc;
  r1 = y0;
  if (r1)
    *radius = r0;
}
void Precompute_Patch_Values(BICUBIC_PATCH *Shape) {
  int i, j;
  VECTOR Control_Points[6];
  for (i = 0; i < 4; i++)
    for (j = 0; j < 4; j++)
      Assign_Vector(Control_Points[4 * i + j], Shape->Control_Points[i][j]);
  find_average(6, Control_Points, Shape->Bounding_Sphere_Center,
               &Shape->Bounding_Sphere_Radius);
}
}
