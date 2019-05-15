/* { dg-do compile } */
/* { dg-options "" } */

typedef struct {
  long long a;
} c;

void e();

void d() {
  c *b;
  switch (b->a)
  case 8:
  case 2:
  case 2057594037927936:
  case 0:
  case 4611686018427387904:
    e();
}
