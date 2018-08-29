/* Test support of scalar_storage_order attribute */

/* { dg-do compile } */

struct __attribute__((scalar_storage_order("little-endian"))) Rec /* { dg-warning "attribute ignored" } */
{
  int i;
};
