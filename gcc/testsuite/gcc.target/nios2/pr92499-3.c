/* PR target/92499 */
/* { dg-do compile } */
/* { dg-options "-O2 -mgpopt=global -G8" } */

/* Check addressing of extern flexibly-sized objects.  */

struct flexible
{
  int length;
  int data[];
};

extern struct flexible extern_flexible;

struct flexible *
get_extern_flexible (void)
{
  return &extern_flexible;
}

/* We should not generate GP-relative addresses for external objects of
   unknown size.  */
/* { dg-final { scan-assembler-not "%gprel\(.*flexible.*\)" } } */
