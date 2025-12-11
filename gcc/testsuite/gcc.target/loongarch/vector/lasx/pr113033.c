/* PR target/113033: ICE with vector left rotate */
/* { dg-do compile } */
/* { dg-options "-O2 -mlasx" } */

typedef unsigned __attribute__ ((vector_size (16))) v4si;
typedef unsigned __attribute__ ((vector_size (32))) v8si;
typedef unsigned long long __attribute__ ((vector_size (16))) v2di;
typedef unsigned long long __attribute__ ((vector_size (32))) v4di;

#define TEST(tp) \
extern tp data_##tp; \
tp \
test_##tp (int x) \
{ \
  const int bit = sizeof (data_##tp[0]) * __CHAR_BIT__; \
  data_##tp = data_##tp << (x & (bit - 1)) \
	      | data_##tp >> (bit - x & (bit - 1)); \
}

TEST (v4si)
TEST (v8si)
TEST (v2di)
TEST (v4di)
