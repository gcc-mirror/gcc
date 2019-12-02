/* PR target/92499 */
/* { dg-do compile } */
/* { dg-options "-O2 -mgpopt=global -G8" } */

/* Check placement and addressing of flexibly-sized objects with external
   linkage.  */

enum { size = 100 };

struct flexible
{
  int length;
  int data[];
};

extern struct flexible global_flexible;
struct flexible global_flexible =
  {
    .data = { [size - 1] = 0, }
  };

extern struct flexible global_flexible_nonzero;
struct flexible global_flexible_nonzero =
  {
    .length = size,
    .data = { [size - 1] = 0, }
  };

struct flexible *
get_global_flexible (void)
{
  return &global_flexible;
}

struct flexible *
get_global_flexible_nonzero (void)
{
  return &global_flexible_nonzero;
}

/* To preserve ABI compatibility we place the flexibly-sized objects in
   small data sections.  */

/* { dg-final { scan-assembler-times "\\.sdata" 1 } } */
/* { dg-final { scan-assembler-times "\\.sbss" 1 } } */
