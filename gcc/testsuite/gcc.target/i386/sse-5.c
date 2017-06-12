/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-prune-output "ABI for passing parameters" } */
/* { dg-options "-Winline -O2 -mno-sse" } */

typedef double v2df __attribute__ ((vector_size (16)));
v2df p;
void q(v2df t) /* { dg-warning "SSE" } */
{
	p=t;
}
