/* { dg-do compile } */
/* { dg-options "-O -mfpmath=sse -msse2" } */
/* { dg-require-effective-target sse2 } */
/* The fact that t1 and t2 are uninitialized is critical.  With them
   uninitialized, the register allocator is free to put them in the same
   hard register, which results in

	xmm0 = xmm0 >= xmm0 ? xmm0 : xmm0

   Which is of course a nop, but one for which we would ICE splitting the
   pattern.  */
   
double out;

static void foo(void)
{
    double t1, t2, t3, t4;
    
    t4 = t1 >= t2 ? t1 : t2;
    t4 = t4 >= t3 ? t4 : t3;
    out = t4;
}
