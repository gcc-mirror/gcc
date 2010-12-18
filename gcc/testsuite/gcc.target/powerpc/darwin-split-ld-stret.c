/* Check for Darwin m64 that we do not try to pass & return by value for a
   struct exceeding the number of arg FPRs (the struct here straddles the 
   split-point).  */
/* { dg-do run { target { powerpc*-*-darwin* && lp64 } } } */

extern void abort (void);

/*#define DEBUG*/

#ifdef DEBUG
extern int printf (const char *, ...);
extern int printf$LDBL128 (const char *, ...);
#endif

typedef struct fourteen {
  long double a, b, c, d, e, f, g;
} fourteen_t ;

fourteen_t foo (fourteen_t, fourteen_t) __attribute__ ((noinline));

fourteen_t 
foo (fourteen_t aa, fourteen_t bb) 
{
  fourteen_t r;

  r.a = aa.a + bb.a;
  r.b = aa.b + bb.b;
  r.c = aa.c + bb.c;
  r.d = aa.d + bb.d;
  r.e = aa.e + bb.e;
  r.f = aa.f + bb.f;
  r.g = aa.g + bb.g;

#ifdef DEBUG
#ifdef __ppc64__
  printf
#else
  printf$LDBL128
#endif	  
	("%Lg %Lg %Lg %Lg %Lg %Lg %Lg %Lg %Lg %Lg %Lg %Lg %Lg %Lg: "
	"%Lg %Lg %Lg %Lg %Lg %Lg %Lg\n",
	aa.a, aa.b, aa.c, aa.d, aa.e, aa.f, aa.g,
	bb.a, bb.b, bb.c, bb.d, bb.e, bb.f, bb.g,
	r.a, r.b, r.c, r.d, r.e, r.f, r.g);
 printf ("aa.g %ll16x %ll16x\nbb.g %ll16x %ll16x\n",
		*(unsigned long long*)&aa.g,
		*(unsigned long long*)(((char *)&aa.g)+8),
		*(unsigned long long*)&bb.g,
		*(unsigned long long*)(((char *)&bb.g)+8));
 
#endif

  __asm__ (""); /* double make sure we don't get inlined */
  return r;
}

int
main (void)
{
  fourteen_t x = { 1.L, 2.L,  3.L,  4.L,  5.L,  6.L,-12.3456789123456789L };
  fourteen_t y = { 8.L, 9.L, 10.L, 11.L, 12.L, 13.L, 12.3456789123456789L };
  fourteen_t z ;
  long double zz;
  
  z = foo (x,y);
  zz = x.g + y.g;
#ifdef DEBUG
#ifdef __ppc64__
	printf
#else
	printf$LDBL128
#endif	  
		("  z: %Lg %Lg %Lg %Lg %Lg %Lg %Lg\n"
		 "ret: %ll16x %ll16x\nzz : %ll16x %ll16x\n",
		z.a, z.b, z.c, z.d, z.e, z.f, z.g,
		*(unsigned long long*)&z.g,
		*(unsigned long long*)(((char *)&z.g)+8),
		*(unsigned long long*)&zz,
		*(unsigned long long*)(((char *)&zz)+8));
#endif

  /* Yes, we really do want to do an equality test here.  */
  if (z.g != zz)
    abort ();

  return 0;
}
