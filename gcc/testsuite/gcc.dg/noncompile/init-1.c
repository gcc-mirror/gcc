struct a
{ double a, b, c; };

struct b
{
  struct a arr[6];
};

static struct b a_b =
{
  {0,0,0},
  {0,0,0},	/* { dg-error "extra brace|excess elements|near" } */
  {0,0,0},	/* { dg-error "extra brace|excess elements|near" } */
  {0,0,0},	/* { dg-error "extra brace|excess elements|near" } */
  {0,0,0},	/* { dg-error "extra brace|excess elements|near" } */
  {0,0,0},	/* { dg-error "extra brace|excess elements|near" } */
};
