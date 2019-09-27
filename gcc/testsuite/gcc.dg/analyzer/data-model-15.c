#include <string.h>

struct coord
{
  double x;
  double y;
  double z;
};

struct tri {
  struct coord verts[3];
};

double test_1 (void)
{
  struct tri t;
  memset (&t, 0, sizeof (struct tri));
  return t.verts[1].y;
}

int test_2 (const struct coord *c1, const struct coord *c2, double r_squared)
{
  double dx = c1->x - c2->x;
  double dy = c1->y - c2->y;
  double dz = c1->z - c2->z;
  return (dx * dx) + (dy * dy) + (dz * dz) <= r_squared;
}

int test_3 (const struct coord *c1, const struct coord *c2, struct coord *out)
{
  out->x = c1->x + c2->x;
  out->y = c1->y + c2->y;
  out->z = c1->z + c2->z;
}
