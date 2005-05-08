/* PR tree-optimization/14841
   Make sure that we can fold a possible nested reference into a
   constant aggregate.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-store_ccp-details" } */

struct car {
  int speed;
  int tire_pressure[4];
};

const struct car cars[] = {
  { 75, { 10, 20, 30, 40 } },
  { 35, { 12, 34, 56, 78 } },
  { 40, { 19, 28, 37, 46 } }
};

extern void link_error (void);

void
foo (void)
{
  if (cars[1].tire_pressure[2] != 56)
    link_error ();
}

/* { dg-final { scan-tree-dump-times "with if \\(0\\)" 1 "store_ccp"} } */
/* { dg-final { cleanup-tree-dump "store_ccp" } } */
