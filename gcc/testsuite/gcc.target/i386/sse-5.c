/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-Winline -Wno-psabi -O2 -mno-sse" } */

typedef double v2df __attribute__ ((vector_size (16)));
v2df p;
q(v2df t) /* { dg-warning "SSE" "" } */
{
	p=t;
}
