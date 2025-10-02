/* { dg-do compile } */
/* { dg-additional-options "-mcpu=tt-ascalon-d8 -mtune=tt-ascalon-d8" } */

/* Verify we don't ICE on the following test cases.  */

typedef char __attribute__((__vector_size__ (32))) vqi;
typedef short __attribute__((__vector_size__ (32))) vhi;
typedef int __attribute__((__vector_size__ (32))) vsi;
typedef long __attribute__((__vector_size__ (32))) vdi;

#define TEST(type) type foo_ ## type (type x, type y) { return x / y; }

TEST(vqi)
TEST(vhi)
TEST(vsi)
TEST(vdi)
