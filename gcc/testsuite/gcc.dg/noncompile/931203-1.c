typedef struct
{
  int x, y;
} point_t;


point_t
f ()
{
  return 0;	/* { dg-error "incompatible types" } */
}
