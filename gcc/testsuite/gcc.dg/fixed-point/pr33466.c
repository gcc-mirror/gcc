/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* The suffix for a fixed-point constant must use a specific order
   for the pieces, and a long long length specifier must use the
   same case (ll or LL).

   These are invalid for all targets, not just those that support
   fixed-point types.  */

long double lLr = 0.5lLr;	/* { dg-error "invalid suffix" } */
long double lLR = 0.5lLR;	/* { dg-error "invalid suffix" } */
long double Llr = 0.5Llr;	/* { dg-error "invalid suffix" } */
long double LlR = 0.5LlR;	/* { dg-error "invalid suffix" } */
long double ulLr = 0.5ulLr;	/* { dg-error "invalid suffix" } */
long double ulLR = 0.5ulLR;	/* { dg-error "invalid suffix" } */
long double uLlr = 0.5uLlr;	/* { dg-error "invalid suffix" } */
long double uLlR = 0.5uLlR;	/* { dg-error "invalid suffix" } */
long double UlLr = 0.5UlLr;	/* { dg-error "invalid suffix" } */
long double UlLR = 0.5UlLR;	/* { dg-error "invalid suffix" } */
long double ULlr = 0.5ULlr;	/* { dg-error "invalid suffix" } */
long double ULlR = 0.5ULlR;	/* { dg-error "invalid suffix" } */
long double lLk = 0.5lLk;	/* { dg-error "invalid suffix" } */
long double lLK = 0.5lLK;	/* { dg-error "invalid suffix" } */
long double Llk = 0.5Llk;	/* { dg-error "invalid suffix" } */
long double LlK = 0.5LlK;	/* { dg-error "invalid suffix" } */
long double ulLk = 0.5ulLk;	/* { dg-error "invalid suffix" } */
long double ulLK = 0.5ulLK;	/* { dg-error "invalid suffix" } */
long double uLlk = 0.5uLlk;	/* { dg-error "invalid suffix" } */
long double uLlK = 0.5uLlK;	/* { dg-error "invalid suffix" } */
long double UlLk = 0.5UlLk;	/* { dg-error "invalid suffix" } */
long double UlLK = 0.5UlLK;	/* { dg-error "invalid suffix" } */
long double ULlk = 0.5ULlk;	/* { dg-error "invalid suffix" } */
long double ULlK = 0.5ULlK;	/* { dg-error "invalid suffix" } */
