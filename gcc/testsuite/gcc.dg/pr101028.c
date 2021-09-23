/* { dg-do compile } */
/* { dg-options "-Ofast" } */

typedef struct {
  double x, y;
} PointInfo;

typedef struct {
  PointInfo point;
} PrimitiveInfo;

int TraceBezier_alpha, TraceBezier_i;
double TraceBezier_weight;
PointInfo *TraceBezier_points;
PrimitiveInfo *TraceBezier_primitive_info;

void TracePath() {
  double *coefficients;
  PointInfo point;
  long j;
  for (; TraceBezier_i; TraceBezier_i++) {
    point.x = point.y = TraceBezier_alpha = 1.0;
    j = 0;
    for (; j < 4; j++) {
      point.x += TraceBezier_alpha * coefficients[j] *
                 TraceBezier_primitive_info->point.x;
      point.y += TraceBezier_alpha * TraceBezier_primitive_info->point.y;
      TraceBezier_alpha *= TraceBezier_weight;
      TraceBezier_primitive_info++;
    }
    TraceBezier_points[TraceBezier_i] = point;
    TraceBezier_weight += 1.0;
  }
}
