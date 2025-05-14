/* Test -Wsizeof-pointer-memaccess warnings.  */
/* { dg-do compile } */
/* { dg-options "-Wall -Wno-array-bounds -Wno-sizeof-array-argument -Wno-stringop-overflow -Wno-stringop-overread" } */

typedef __SIZE_TYPE__ size_t;
extern void bzero (void *, size_t);
extern void bcopy (void *, const void *, size_t);
extern int bcmp (const void *, const void *, size_t);

struct A { short a, b; int c, d; long e, f; };
typedef struct A TA;
typedef struct A *PA;
typedef TA *PTA;
struct B {};
typedef struct B TB;
typedef struct B *PB;
typedef TB *PTB;
typedef int X[3][3][3];

int
f1 (void *x, int z)
{
  struct A a, *pa1 = &a;
  TA *pa2 = &a;
  PA pa3 = &a;
  PTA pa4 = &a;
  bzero (&a, sizeof (&a));		    /* { dg-warning "call is the same expression as the destination; did you mean to remove the addressof" } */
  bzero (pa1, sizeof (pa1));		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  bzero (pa2, sizeof pa2);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  bzero (pa3, sizeof (pa3));		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  bzero (pa4, sizeof pa4);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  bzero (pa1, sizeof (struct A *));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  bzero (pa2, sizeof (PTA));		    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  bzero (pa3, sizeof (PA));		    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  bzero (pa4, sizeof (__typeof (pa4)));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */

  bcopy (x, &a, sizeof (&a));		    /* { dg-warning "call is the same expression as the destination; did you mean to remove the addressof" } */
  bcopy (x, pa1, sizeof (pa1));		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  bcopy (x, pa2, sizeof pa2);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  bcopy (x, pa3, sizeof (pa3));		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  bcopy (x, pa4, sizeof pa4);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  bcopy (x, pa1, sizeof (struct A *));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  bcopy (x, pa2, sizeof (PTA));		    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  bcopy (x, pa3, sizeof (PA));		    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  bcopy (x, pa4, sizeof (__typeof (pa4)));  /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */

  bcopy (&a, x, sizeof (&a));		    /* { dg-warning "call is the same expression as the source; did you mean to remove the addressof" } */
  bcopy (pa1, x, sizeof (pa1));		    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  bcopy (pa2, x, sizeof pa2);		    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  bcopy (pa3, x, sizeof (pa3));		    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  bcopy (pa4, x, sizeof pa4);		    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  bcopy (pa1, x, sizeof (struct A *));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  bcopy (pa2, x, sizeof (PTA));		    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  bcopy (pa3, x, sizeof (PA));		    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  bcopy (pa4, x, sizeof (__typeof (pa4)));  /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */

  z += bcmp (&a, x, sizeof (&a));	    /* { dg-warning "call is the same expression as the first source; did you mean to remove the addressof" } */
  z += bcmp (pa1, x, sizeof (pa1));	    /* { dg-warning "call is the same expression as the first source; did you mean to dereference it" } */
  z += bcmp (pa2, x, sizeof pa2);	    /* { dg-warning "call is the same expression as the first source; did you mean to dereference it" } */
  z += bcmp (pa3, x, sizeof (pa3));	    /* { dg-warning "call is the same expression as the first source; did you mean to dereference it" } */
  z += bcmp (pa4, x, sizeof pa4);	    /* { dg-warning "call is the same expression as the first source; did you mean to dereference it" } */
  z += bcmp (pa1, x, sizeof (struct A *));  /* { dg-warning "call is the same pointer type \[^\n\r\]* as the first source; expected \[^\n\r\]* or an explicit length" } */
  z += bcmp (pa2, x, sizeof (PTA));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the first source; expected \[^\n\r\]* or an explicit length" } */
  z += bcmp (pa3, x, sizeof (PA));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the first source; expected \[^\n\r\]* or an explicit length" } */

  z += bcmp (x, &a, sizeof (&a));	    /* { dg-warning "call is the same expression as the second source; did you mean to remove the addressof" } */
  z += bcmp (x, pa1, sizeof (pa1));	    /* { dg-warning "call is the same expression as the second source; did you mean to dereference it" } */
  z += bcmp (x, pa2, sizeof pa2);	    /* { dg-warning "call is the same expression as the second source; did you mean to dereference it" } */
  z += bcmp (x, pa3, sizeof (pa3));	    /* { dg-warning "call is the same expression as the second source; did you mean to dereference it" } */
  z += bcmp (x, pa4, sizeof pa4);	    /* { dg-warning "call is the same expression as the second source; did you mean to dereference it" } */
  z += bcmp (x, pa1, sizeof (struct A *));  /* { dg-warning "call is the same pointer type \[^\n\r\]* as the second source; expected \[^\n\r\]* or an explicit length" } */
  z += bcmp (x, pa2, sizeof (PTA));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the second source; expected \[^\n\r\]* or an explicit length" } */
  z += bcmp (x, pa3, sizeof (PA));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the second source; expected \[^\n\r\]* or an explicit length" } */

  z += bcmp (x, (&a), (sizeof (&a)));	    /* { dg-warning "call is the same expression as the second source; did you mean to remove the addressof" } */
  z += bcmp (x, (pa1), (sizeof (pa1)));	    /* { dg-warning "call is the same expression as the second source; did you mean to dereference it" } */
  z += bcmp (x, (pa2), (sizeof pa2));	    /* { dg-warning "call is the same expression as the second source; did you mean to dereference it" } */
  z += bcmp (x, (pa3), (sizeof (pa3)));	    /* { dg-warning "call is the same expression as the second source; did you mean to dereference it" } */
  z += bcmp (x, (pa4), (sizeof pa4));	    /* { dg-warning "call is the same expression as the second source; did you mean to dereference it" } */
  z += bcmp (x, (pa1), (sizeof (struct A *)));/* { dg-warning "call is the same pointer type \[^\n\r\]* as the second source; expected \[^\n\r\]* or an explicit length" } */
  z += bcmp (x, (pa2), (sizeof (PTA)));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the second source; expected \[^\n\r\]* or an explicit length" } */
  z += bcmp (x, (pa3), (sizeof (PA)));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the second source; expected \[^\n\r\]* or an explicit length" } */

  /* These are correct, no warning.  */
  bzero (&a, sizeof a);
  bzero (&a, sizeof (a));
  bzero (&a, sizeof (struct A));
  bzero (&a, sizeof (const struct A));
  bzero (&a, sizeof (volatile struct A));
  bzero (&a, sizeof (volatile const struct A));
  bzero (&a, sizeof (TA));
  bzero (&a, sizeof (__typeof (*&a)));
  bzero (pa1, sizeof (*pa1));
  bzero (pa2, sizeof (*pa3));
  bzero (pa3, sizeof (__typeof (*pa3)));
  /* These are probably broken, but obfuscated, no warning.  */
  bzero ((void *) &a, sizeof (&a));
  bzero ((char *) &a, sizeof (&a));
  bzero (&a, sizeof (&a) + 0);
  bzero (&a, 0 + sizeof (&a));

  /* These are correct, no warning.  */
  bcopy (x, &a, sizeof a);
  bcopy (x, &a, sizeof (a));
  bcopy (x, &a, sizeof (struct A));
  bcopy (x, &a, sizeof (const struct A));
  bcopy (x, &a, sizeof (volatile struct A));
  bcopy (x, &a, sizeof (volatile const struct A));
  bcopy (x, &a, sizeof (TA));
  bcopy (x, &a, sizeof (__typeof (*&a)));
  bcopy (x, pa1, sizeof (*pa1));
  bcopy (x, pa2, sizeof (*pa3));
  bcopy (x, pa3, sizeof (__typeof (*pa3)));
  /* These are probably broken, but obfuscated, no warning.  */
  bcopy (x, (void *) &a, sizeof (&a));
  bcopy (x, (char *) &a, sizeof (&a));
  bcopy (x, &a, sizeof (&a) + 0);
  bcopy (x, &a, 0 + sizeof (&a));

  /* These are correct, no warning.  */
  bcopy (&a, x, sizeof a);
  bcopy (&a, x, sizeof (a));
  bcopy (&a, x, sizeof (struct A));
  bcopy (&a, x, sizeof (const struct A));
  bcopy (&a, x, sizeof (volatile struct A));
  bcopy (&a, x, sizeof (volatile const struct A));
  bcopy (&a, x, sizeof (TA));
  bcopy (&a, x, sizeof (__typeof (*&a)));
  bcopy (pa1, x, sizeof (*pa1));
  bcopy (pa2, x, sizeof (*pa3));
  bcopy (pa3, x, sizeof (__typeof (*pa3)));
  /* These are probably broken, but obfuscated, no warning.  */
  bcopy ((void *) &a, x, sizeof (&a));
  bcopy ((char *) &a, x, sizeof (&a));
  bcopy (&a, x, sizeof (&a) + 0);
  bcopy (&a, x, 0 + sizeof (&a));

  /* These are correct, no warning.  */
  z += bcmp (&a, x, sizeof a);
  z += bcmp (&a, x, sizeof (a));
  z += bcmp (&a, x, sizeof (struct A));
  z += bcmp (&a, x, sizeof (const struct A));
  z += bcmp (&a, x, sizeof (volatile struct A));
  z += bcmp (&a, x, sizeof (volatile const struct A));
  z += bcmp (&a, x, sizeof (TA));
  z += bcmp (&a, x, sizeof (__typeof (*&a)));
  z += bcmp (pa1, x, sizeof (*pa1));
  z += bcmp (pa2, x, sizeof (*pa3));
  z += bcmp (pa3, x, sizeof (__typeof (*pa3)));
  /* These are probably broken, but obfuscated, no warning.  */
  z += bcmp ((void *) &a, x, sizeof (&a));
  z += bcmp ((char *) &a, x, sizeof (&a));
  z += bcmp (&a, x, sizeof (&a) + 0);
  z += bcmp (&a, x, 0 + sizeof (&a));

  /* These are correct, no warning.  */
  z += bcmp (x, &a, sizeof a);
  z += bcmp (x, &a, sizeof (a));
  z += bcmp (x, &a, sizeof (struct A));
  z += bcmp (x, &a, sizeof (const struct A));
  z += bcmp (x, &a, sizeof (volatile struct A));
  z += bcmp (x, &a, sizeof (volatile const struct A));
  z += bcmp (x, &a, sizeof (TA));
  z += bcmp (x, &a, sizeof (__typeof (*&a)));
  z += bcmp (x, pa1, sizeof (*pa1));
  z += bcmp (x, pa2, sizeof (*pa3));
  z += bcmp (x, pa3, sizeof (__typeof (*pa3)));
  /* These are probably broken, but obfuscated, no warning.  */
  z += bcmp (x, (void *) &a, sizeof (&a));
  z += bcmp (x, (char *) &a, sizeof (&a));
  z += bcmp (x, &a, sizeof (&a) + 0);
  z += bcmp (x, &a, 0 + sizeof (&a));

  return z;
}

int
f2 (void *x, int z)
{
  struct B b, *pb1 = &b;
  TB *pb2 = &b;
  PB pb3 = &b;
  PTB pb4 = &b;
  bzero (&b, sizeof (&b));		    /* { dg-warning "call is the same expression as the destination; did you mean to remove the addressof" } */
  bzero (pb1, sizeof (pb1));		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  bzero (pb2, sizeof pb2);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  bzero (pb3, sizeof (pb3));		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  bzero (pb4, sizeof pb4);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  bzero (pb1, sizeof (struct B *));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  bzero (pb2, sizeof (PTB));		    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  bzero (pb3, sizeof (PB));		    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  bzero (pb4, sizeof (__typeof (pb4)));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */

  bcopy (x, &b, sizeof (&b));		    /* { dg-warning "call is the same expression as the destination; did you mean to remove the addressof" } */
  bcopy (x, pb1, sizeof (pb1));		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  bcopy (x, pb2, sizeof pb2);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  bcopy (x, pb3, sizeof (pb3));		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  bcopy (x, pb4, sizeof pb4);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  bcopy (x, pb1, sizeof (struct B *));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  bcopy (x, pb2, sizeof (PTB));		    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  bcopy (x, pb3, sizeof (PB));		    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  bcopy (x, pb4, sizeof (__typeof (pb4)));  /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */

  bcopy (&b, x, sizeof (&b));		    /* { dg-warning "call is the same expression as the source; did you mean to remove the addressof" } */
  bcopy (pb1, x, sizeof (pb1));		    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  bcopy (pb2, x, sizeof pb2);		    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  bcopy (pb3, x, sizeof (pb3));		    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  bcopy (pb4, x, sizeof pb4);		    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  bcopy (pb1, x, sizeof (struct B *));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  bcopy (pb2, x, sizeof (PTB));		    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  bcopy (pb3, x, sizeof (PB));		    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  bcopy (pb4, x, sizeof (__typeof (pb4)));  /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */

  z += bcmp (&b, x, sizeof (&b));	    /* { dg-warning "call is the same expression as the first source; did you mean to remove the addressof" } */
  z += bcmp (pb1, x, sizeof (pb1));	    /* { dg-warning "call is the same expression as the first source; did you mean to dereference it" } */
  z += bcmp (pb2, x, sizeof pb2);	    /* { dg-warning "call is the same expression as the first source; did you mean to dereference it" } */
  z += bcmp (pb3, x, sizeof (pb3));	    /* { dg-warning "call is the same expression as the first source; did you mean to dereference it" } */
  z += bcmp (pb4, x, sizeof pb4);	    /* { dg-warning "call is the same expression as the first source; did you mean to dereference it" } */
  z += bcmp (pb1, x, sizeof (struct B *));  /* { dg-warning "call is the same pointer type \[^\n\r\]* as the first source; expected \[^\n\r\]* or an explicit length" } */
  z += bcmp (pb2, x, sizeof (PTB));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the first source; expected \[^\n\r\]* or an explicit length" } */
  z += bcmp (pb3, x, sizeof (PB));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the first source; expected \[^\n\r\]* or an explicit length" } */

  z += bcmp (x, &b, sizeof (&b));	    /* { dg-warning "call is the same expression as the second source; did you mean to remove the addressof" } */
  z += bcmp (x, pb1, sizeof (pb1));	    /* { dg-warning "call is the same expression as the second source; did you mean to dereference it" } */
  z += bcmp (x, pb2, sizeof pb2);	    /* { dg-warning "call is the same expression as the second source; did you mean to dereference it" } */
  z += bcmp (x, pb3, sizeof (pb3));	    /* { dg-warning "call is the same expression as the second source; did you mean to dereference it" } */
  z += bcmp (x, pb4, sizeof pb4);	    /* { dg-warning "call is the same expression as the second source; did you mean to dereference it" } */
  z += bcmp (x, pb1, sizeof (struct B *));  /* { dg-warning "call is the same pointer type \[^\n\r\]* as the second source; expected \[^\n\r\]* or an explicit length" } */
  z += bcmp (x, pb2, sizeof (PTB));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the second source; expected \[^\n\r\]* or an explicit length" } */
  z += bcmp (x, pb3, sizeof (PB));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the second source; expected \[^\n\r\]* or an explicit length" } */

  /* These are correct, no warning.  */
  bzero (&b, sizeof b);
  bzero (&b, sizeof (b));
  bzero (&b, sizeof (struct B));
  bzero (&b, sizeof (const struct B));
  bzero (&b, sizeof (volatile struct B));
  bzero (&b, sizeof (volatile const struct B));
  bzero (&b, sizeof (TB));
  bzero (&b, sizeof (__typeof (*&b)));
  bzero (pb1, sizeof (*pb1));
  bzero (pb2, sizeof (*pb3));
  bzero (pb3, sizeof (__typeof (*pb3)));
  /* These are probably broken, but obfuscated, no warning.  */
  bzero ((void *) &b, sizeof (&b));
  bzero ((char *) &b, sizeof (&b));
  bzero (&b, sizeof (&b) + 0);
  bzero (&b, 0 + sizeof (&b));

  /* These are correct, no warning.  */
  bcopy (x, &b, sizeof b);
  bcopy (x, &b, sizeof (b));
  bcopy (x, &b, sizeof (struct B));
  bcopy (x, &b, sizeof (const struct B));
  bcopy (x, &b, sizeof (volatile struct B));
  bcopy (x, &b, sizeof (volatile const struct B));
  bcopy (x, &b, sizeof (TB));
  bcopy (x, &b, sizeof (__typeof (*&b)));
  bcopy (x, pb1, sizeof (*pb1));
  bcopy (x, pb2, sizeof (*pb3));
  bcopy (x, pb3, sizeof (__typeof (*pb3)));
  /* These are probably broken, but obfuscated, no warning.  */
  bcopy (x, (void *) &b, sizeof (&b));
  bcopy (x, (char *) &b, sizeof (&b));
  bcopy (x, &b, sizeof (&b) + 0);
  bcopy (x, &b, 0 + sizeof (&b));

  /* These are correct, no warning.  */
  bcopy (&b, x, sizeof b);
  bcopy (&b, x, sizeof (b));
  bcopy (&b, x, sizeof (struct B));
  bcopy (&b, x, sizeof (const struct B));
  bcopy (&b, x, sizeof (volatile struct B));
  bcopy (&b, x, sizeof (volatile const struct B));
  bcopy (&b, x, sizeof (TB));
  bcopy (&b, x, sizeof (__typeof (*&b)));
  bcopy (pb1, x, sizeof (*pb1));
  bcopy (pb2, x, sizeof (*pb3));
  bcopy (pb3, x, sizeof (__typeof (*pb3)));
  /* These are probably broken, but obfuscated, no warning.  */
  bcopy ((void *) &b, x, sizeof (&b));
  bcopy ((char *) &b, x, sizeof (&b));
  bcopy (&b, x, sizeof (&b) + 0);
  bcopy (&b, x, 0 + sizeof (&b));

  /* These are correct, no warning.  */
  z += bcmp (&b, x, sizeof b);
  z += bcmp (&b, x, sizeof (b));
  z += bcmp (&b, x, sizeof (struct B));
  z += bcmp (&b, x, sizeof (const struct B));
  z += bcmp (&b, x, sizeof (volatile struct B));
  z += bcmp (&b, x, sizeof (volatile const struct B));
  z += bcmp (&b, x, sizeof (TB));
  z += bcmp (&b, x, sizeof (__typeof (*&b)));
  z += bcmp (pb1, x, sizeof (*pb1));
  z += bcmp (pb2, x, sizeof (*pb3));
  z += bcmp (pb3, x, sizeof (__typeof (*pb3)));
  /* These are probably broken, but obfuscated, no warning.  */
  z += bcmp ((void *) &b, x, sizeof (&b));
  z += bcmp ((char *) &b, x, sizeof (&b));
  z += bcmp (&b, x, sizeof (&b) + 0);
  z += bcmp (&b, x, 0 + sizeof (&b));

  /* These are correct, no warning.  */
  z += bcmp (x, &b, sizeof b);
  z += bcmp (x, &b, sizeof (b));
  z += bcmp (x, &b, sizeof (struct B));
  z += bcmp (x, &b, sizeof (const struct B));
  z += bcmp (x, &b, sizeof (volatile struct B));
  z += bcmp (x, &b, sizeof (volatile const struct B));
  z += bcmp (x, &b, sizeof (TB));
  z += bcmp (x, &b, sizeof (__typeof (*&b)));
  z += bcmp (x, pb1, sizeof (*pb1));
  z += bcmp (x, pb2, sizeof (*pb3));
  z += bcmp (x, pb3, sizeof (__typeof (*pb3)));
  /* These are probably broken, but obfuscated, no warning.  */
  z += bcmp (x, (void *) &b, sizeof (&b));
  z += bcmp (x, (char *) &b, sizeof (&b));
  z += bcmp (x, &b, sizeof (&b) + 0);
  z += bcmp (x, &b, 0 + sizeof (&b));

  return z;
}

int
f3 (void *x, char *y, int z, X w)
{
  unsigned char *y1 = (unsigned char *) __builtin_alloca (z + 16);
  char buf1[7];
  signed char buf2[z + 32];
  long buf3[17];
  int *buf4[9];
  signed char *y2 = buf2;
  char c;
  char *y3;
  bzero (y, sizeof (y));		    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */
  bzero (y1, sizeof (y1));		    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */
  bzero (y2, sizeof (y2));		    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */
  bzero (&c, sizeof (&c));		    /* { dg-warning "call is the same expression as the destination; did you mean to remove the addressof" } */
  bzero (w, sizeof w);			    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */

  bcopy (x, y, sizeof (y));		    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */
  bcopy (x, y1, sizeof (y1));		    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */
  bcopy (x, y2, sizeof (y2));		    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */
  bcopy (x, &c, sizeof (&c));		    /* { dg-warning "call is the same expression as the destination; did you mean to remove the addressof" } */
  bcopy (x, w, sizeof w);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */

  bcopy (y, x, sizeof (y));		    /* { dg-warning "call is the same expression as the source; did you mean to provide an explicit length" } */
  bcopy (y1, x, sizeof (y1));		    /* { dg-warning "call is the same expression as the source; did you mean to provide an explicit length" } */
  bcopy (y2, x, sizeof (y2));		    /* { dg-warning "call is the same expression as the source; did you mean to provide an explicit length" } */
  bcopy (&c, x, sizeof (&c));		    /* { dg-warning "call is the same expression as the source; did you mean to remove the addressof" } */
  bcopy (w, x, sizeof w);		    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */

  z += bcmp (y, x, sizeof (y));		    /* { dg-warning "call is the same expression as the first source; did you mean to provide an explicit length" } */
  z += bcmp (y1, x, sizeof (y1));	    /* { dg-warning "call is the same expression as the first source; did you mean to provide an explicit length" } */
  z += bcmp (y2, x, sizeof (y2));	    /* { dg-warning "call is the same expression as the first source; did you mean to provide an explicit length" } */
  z += bcmp (&c, x, sizeof (&c));	    /* { dg-warning "call is the same expression as the first source; did you mean to remove the addressof" } */
  z += bcmp (w, x, sizeof w);		    /* { dg-warning "call is the same expression as the first source; did you mean to dereference it" } */

  z += bcmp (x, y, sizeof (y));		    /* { dg-warning "call is the same expression as the second source; did you mean to provide an explicit length" } */
  z += bcmp (x, y1, sizeof (y1));	    /* { dg-warning "call is the same expression as the second source; did you mean to provide an explicit length" } */
  z += bcmp (x, y2, sizeof (y2));	    /* { dg-warning "call is the same expression as the second source; did you mean to provide an explicit length" } */
  z += bcmp (x, &c, sizeof (&c));	    /* { dg-warning "call is the same expression as the second source; did you mean to remove the addressof" } */
  z += bcmp (x, w, sizeof w);		    /* { dg-warning "call is the same expression as the second source; did you mean to dereference it" } */

  /* These are correct, no warning.  */
  bzero (y, sizeof (*y));
  bzero (y1, sizeof (*y2));
  bzero (buf1, sizeof buf1);
  bzero (buf3, sizeof (buf3));
  bzero (&buf3[0], sizeof (buf3));
  bzero (&buf4[0], sizeof (buf4));
  bzero (w, sizeof (X));
  /* These are probably broken, but obfuscated, no warning.  */
  bzero ((void *) y, sizeof (y));
  bzero ((char *) y1, sizeof (y2));
  bzero (y, sizeof (y) + 0);
  bzero (y1, 0 + sizeof (y2));
  bzero ((void *) &c, sizeof (&c));
  bzero ((signed char *) &c, sizeof (&c));
  bzero (&c, sizeof (&c) + 0);
  bzero (&c, 0 + sizeof (&c));

  /* These are correct, no warning.  */
  bcopy (x, y, sizeof (*y));
  bcopy (x, y1, sizeof (*y2));
  bcopy (x, buf1, sizeof buf1);
  bcopy (x, buf3, sizeof (buf3));
  bcopy (x, &buf3[0], sizeof (buf3));
  bcopy (x, &buf4[0], sizeof (buf4));
  bcopy (y, &y3, sizeof (y3));
  bcopy (y, (char *) &y3, sizeof (y3));
  bcopy (x, w, sizeof (X));
  /* These are probably broken, but obfuscated, no warning.  */
  bcopy (x, (void *) y, sizeof (y));
  bcopy (x, (char *) y1, sizeof (y2));
  bcopy (x, y, sizeof (y) + 0);
  bcopy (x, y1, 0 + sizeof (y2));
  bcopy (x, (void *) &c, sizeof (&c));
  bcopy (x, (signed char *) &c, sizeof (&c));
  bcopy (x, &c, sizeof (&c) + 0);
  bcopy (x, &c, 0 + sizeof (&c));

  /* These are correct, no warning.  */
  bcopy (y, x, sizeof (*y));
  bcopy (y1, x, sizeof (*y2));
  bcopy (buf1, x, sizeof buf1);
  bcopy (buf3, x, sizeof (buf3));
  bcopy (&buf3[0], x, sizeof (buf3));
  bcopy (&buf4[0], x, sizeof (buf4));
  bcopy (&y3, y, sizeof (y3));
  bcopy ((char *) &y3, y, sizeof (y3));
  bcopy (w, x, sizeof (X));
  /* These are probably broken, but obfuscated, no warning.  */
  bcopy ((void *) y, x, sizeof (y));
  bcopy ((char *) y1, x, sizeof (y2));
  bcopy (y, x, sizeof (y) + 0);
  bcopy (y1, x, 0 + sizeof (y2));
  bcopy ((void *) &c, x, sizeof (&c));
  bcopy ((signed char *) &c, x, sizeof (&c));
  bcopy (&c, x, sizeof (&c) + 0);
  bcopy (&c, x, 0 + sizeof (&c));

  /* These are correct, no warning.  */
  z += bcmp (y, x, sizeof (*y));
  z += bcmp (y1, x, sizeof (*y2));
  z += bcmp (buf1, x, sizeof buf1);
  z += bcmp (buf3, x, sizeof (buf3));
  z += bcmp (&buf3[0], x, sizeof (buf3));
  z += bcmp (&buf4[0], x, sizeof (buf4));
  z += bcmp (&y3, y, sizeof (y3));
  z += bcmp ((char *) &y3, y, sizeof (y3));
  z += bcmp (w, x, sizeof (X));
  /* These are probably broken, but obfuscated, no warning.  */
  z += bcmp ((void *) y, x, sizeof (y));
  z += bcmp ((char *) y1, x, sizeof (y2));
  z += bcmp (y, x, sizeof (y) + 0);
  z += bcmp (y1, x, 0 + sizeof (y2));
  z += bcmp ((void *) &c, x, sizeof (&c));
  z += bcmp ((signed char *) &c, x, sizeof (&c));
  z += bcmp (&c, x, sizeof (&c) + 0);
  z += bcmp (&c, x, 0 + sizeof (&c));

  /* These are correct, no warning.  */
  z += bcmp (x, y, sizeof (*y));
  z += bcmp (x, y1, sizeof (*y2));
  z += bcmp (x, buf1, sizeof buf1);
  z += bcmp (x, buf3, sizeof (buf3));
  z += bcmp (x, &buf3[0], sizeof (buf3));
  z += bcmp (x, &buf4[0], sizeof (buf4));
  z += bcmp (y, &y3, sizeof (y3));
  z += bcmp (y, (char *) &y3, sizeof (y3));
  z += bcmp (x, w, sizeof (X));
  /* These are probably broken, but obfuscated, no warning.  */
  z += bcmp (x, (void *) y, sizeof (y));
  z += bcmp (x, (char *) y1, sizeof (y2));
  z += bcmp (x, y, sizeof (y) + 0);
  z += bcmp (x, y1, 0 + sizeof (y2));
  z += bcmp (x, (void *) &c, sizeof (&c));
  z += bcmp (x, (signed char *) &c, sizeof (&c));
  z += bcmp (x, &c, sizeof (&c) + 0);
  z += bcmp (x, &c, 0 + sizeof (&c));

  return z;
}
