/* PR target/92499 */
/* { dg-do compile } */
/* { dg-options "-O2 -mgpopt=global -G8" } */

/* Check placement and addressing of flexibly-sized objects with internal
   linkage.  */

enum { size = 100 };

struct flexible
{
  int length;
  int data[];
};

static struct flexible local_flexible =
  {
    .data = { [size - 1] = 0, }
  };

static struct flexible local_flexible_nonzero =
  {
    .length = size,
    .data = { [size - 1] = 0, }
  };

struct flexible *
get_local_flexible (void)
{
  return &local_flexible;
}

struct flexible *
get_local_flexible_nonzero (void)
{
  return &local_flexible_nonzero;
}

/* We should not place the flexibly-sized objects in small data
   sections, or generate gp-relative addresses for them.  */

/* { dg-final { scan-assembler-not "\\.sdata" } } */
/* { dg-final { scan-assembler-not "\\.sbss" } } */
/* { dg-final { scan-assembler-not "%gprel\(.*flexible.*\)" } } */




