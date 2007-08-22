/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-Winline -O2 -march=i386" } */

typedef double v2df __attribute__ ((vector_size (16)));
v2df p;
q(v2df t)
{			 /* { dg-warning "SSE" "" } */
	p=t;
}
