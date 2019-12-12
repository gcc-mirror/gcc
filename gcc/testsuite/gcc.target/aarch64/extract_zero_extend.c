/* { dg-do compile } */
/* { dg-options "-O3 -fdump-rtl-final" } */

/* Tests div16qi.  */
typedef unsigned char div16qi __attribute__ ((vector_size (16)));
/* Tests div8qi.  */
typedef unsigned char div8qi __attribute__ ((vector_size (8)));
/* Tests div8hi.  */
typedef unsigned short div8hi __attribute__ ((vector_size (16)));
/* Tests div4hi.  */
typedef unsigned short div4hi __attribute__ ((vector_size (8)));

/* Tests siv16qi.  */
typedef unsigned char siv16qi __attribute__ ((vector_size (16)));
/* Tests siv8qi.  */
typedef unsigned char siv8qi __attribute__ ((vector_size (8)));
/* Tests siv8hi.  */
typedef unsigned short siv8hi __attribute__ ((vector_size (16)));
/* Tests siv4hi.  */
typedef unsigned short siv4hi __attribute__ ((vector_size (8)));


unsigned long long
foo_div16qi (div16qi a)
{
  return a[0];
}

unsigned long long
foo_div8qi (div8qi a)
{
  return a[0];
}

unsigned long long
foo_div8hi (div8hi a)
{
  return a[0];
}

unsigned long long
foo_div4hi (div4hi a)
{
  return a[0];
}

unsigned int
foo_siv16qi (siv16qi a)
{
  return a[0];
}

unsigned int
foo_siv8qi (siv8qi a)
{
  return a[0];
}

unsigned int
foo_siv8hi (siv8hi a)
{
  return a[0];
}

unsigned int
foo_siv4hi (siv4hi a)
{
  return a[0];
}

/* { dg-final { scan-assembler-times "umov\\t" 8 } } */
/* { dg-final { scan-assembler-not "and\\t" } } */

/* { dg-final { scan-rtl-dump "aarch64_get_lane_zero_extenddiv16qi" "final" } } */
/* { dg-final { scan-rtl-dump "aarch64_get_lane_zero_extenddiv8qi" "final" } } */
/* { dg-final { scan-rtl-dump "aarch64_get_lane_zero_extenddiv8hi" "final" } } */
/* { dg-final { scan-rtl-dump "aarch64_get_lane_zero_extenddiv4hi" "final" } } */
/* { dg-final { scan-rtl-dump "aarch64_get_lane_zero_extendsiv16qi" "final" } } */
/* { dg-final { scan-rtl-dump "aarch64_get_lane_zero_extendsiv8qi" "final" } } */
/* { dg-final { scan-rtl-dump "aarch64_get_lane_zero_extendsiv8hi" "final" } } */
/* { dg-final { scan-rtl-dump "aarch64_get_lane_zero_extendsiv4hi" "final" } } */
