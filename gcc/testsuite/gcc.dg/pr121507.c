/* { dg-do compile } */
/* { dg-options "-std=c23" } */

int * xmalloc(...);	
int * xmalloc(n)	/* { dg-error "conflicting types" } */
        int n;		/* { dg-warning "old-style function definition" "" { target *-*-* } .-1 } */
{
  return 0;
}

