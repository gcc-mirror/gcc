/* Test support of scalar_storage_order attribute */

/* { dg-do compile } */

struct S3
{
  struct __attribute__((scalar_storage_order("big-endian"))) S1
  {
    int i;
  } s1;
};

struct S4
{
  struct __attribute__((scalar_storage_order("little-endian"))) S2
  {
    int i;
  } s2;
};

void incompatible_assign (struct S3 *s3, struct S4 *s4)
{
  s3->s1 = s4->s2; /* { dg-error "(incompatible types|no match)" } */
}
