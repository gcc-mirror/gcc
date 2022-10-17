/* PR c++/55095 */
/* { dg-do compile { target int32 } } */
/* { dg-options "-O -Wshift-overflow=2" } */

#define INTM1 (sizeof (int) * __CHAR_BIT__ - 1)
#define LLONGM1 (sizeof (long long) * __CHAR_BIT__ - 1)

int i1 = 1 << INTM1; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
unsigned u1 = 1 << INTM1; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
long long int l1 = 1LL << LLONGM1; /* { dg-warning "requires 65 bits to represent" "" { target { c || c++11_down } } } */
