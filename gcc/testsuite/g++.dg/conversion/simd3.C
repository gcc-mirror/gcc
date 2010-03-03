/* { dg-do compile } */

typedef int myint;

float __attribute__((vector_size(16))) b;
int __attribute__((vector_size(16))) d;
myint __attribute__((vector_size(16))) d2;
unsigned int __attribute__((vector_size(16))) e;

void foo()
{
	b + d; /* { dg-error "invalid operands to binary" } */
	d += e; /* { dg-message "use -flax-vector-conversions to permit conversions between vectors with differing element types or numbers of subparts.*cannot convert 'unsigned int __vector.4.' to 'int __vector.4.' in assignment" } */
	d2 += d;
}
