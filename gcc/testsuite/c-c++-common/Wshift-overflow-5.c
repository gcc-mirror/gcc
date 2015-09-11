/* PR c++/55095 */
/* { dg-do compile { target int32 } } */
/* { dg-options "-O -Wshift-overflow=2" } */
/* { dg-additional-options "-std=c++11" { target c++ } } */

#define INTM1 (sizeof (int) * __CHAR_BIT__ - 1)
#define LLONGM1 (sizeof (long long) * __CHAR_BIT__ - 1)

int i1 = 1 << INTM1; /* { dg-warning "requires 33 bits to represent" } */
unsigned u1 = 1 << INTM1; /* { dg-warning "requires 33 bits to represent" } */
long long int l1 = 1LL << LLONGM1; /* { dg-warning "requires 65 bits to represent" } */
