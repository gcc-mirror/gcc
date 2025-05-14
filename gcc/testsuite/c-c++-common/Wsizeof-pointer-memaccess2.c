/* Test -Wsizeof-pointer-memaccess warnings.  */
/* { dg-do compile } */
/* { dg-options "-Wall -O2 -Wno-array-bounds -Wno-sizeof-array-argument -Wno-stringop-truncation -ftrack-macro-expansion=0" } */
/* { dg-options "-Wall -O2 -Wno-array-bounds -Wno-sizeof-array-argument -Wno-stringop-truncation -Wno-c++-compat -ftrack-macro-expansion=0" {target c} } */

#define bos(ptr) __builtin_object_size (ptr, 1)
#define bos0(ptr) __builtin_object_size (ptr, 0)

#define memset(dst, val, sz) __builtin___memset_chk (dst, val, sz, bos (dst))
#define memcpy(dst, src, sz) __builtin___memcpy_chk (dst, src, sz, bos (dst))
#define memmove(dst, src, sz) __builtin___memmove_chk (dst, src, sz, bos (dst))
#define strncpy(dst, src, sz) __builtin___strncpy_chk (dst, src, sz, bos (dst))
#define strncat(dst, src, sz) __builtin___strncat_chk (dst, src, sz, bos (dst))
#define stpncpy(dst, src, sz) __builtin___stpncpy_chk (dst, src, sz, bos (dst))

struct A { short a, b; int c, d; long e, f; };
typedef struct A TA;
typedef struct A *PA;
typedef TA *PTA;
struct B {};
typedef struct B TB;
typedef struct B *PB;
typedef TB *PTB;
typedef int X[3][3][3];

void
f1 (void *x)
{
  struct A a, *pa1 = &a;
  TA *pa2 = &a;
  PA pa3 = &a;
  PTA pa4 = &a;
  memset (&a, 0, sizeof (&a));		    /* { dg-warning "call is the same expression as the destination; did you mean to remove the addressof" } */
  memset (pa1, 0, sizeof (pa1));	    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memset (pa2, 0, sizeof pa2);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memset (pa3, 0, sizeof (pa3));	    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memset (pa4, 0, sizeof pa4);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memset (pa1, 0, sizeof (struct A *));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  memset (pa2, 0, sizeof (PTA));    	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  memset (pa3, 0, sizeof (PA));		    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  memset (pa4, 0, sizeof (__typeof (pa4))); /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */

  memcpy (&a, x, sizeof (&a));		    /* { dg-warning "call is the same expression as the destination; did you mean to remove the addressof" } */
  memcpy (pa1, x, sizeof (pa1));	    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memcpy (pa2, x, sizeof pa2);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memcpy (pa3, x, sizeof (pa3));	    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memcpy (pa4, x, sizeof pa4);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memcpy (pa1, x, sizeof (struct A *));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  memcpy (pa2, x, sizeof (PTA));    	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  memcpy (pa3, x, sizeof (PA));		    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  memcpy (pa4, x, sizeof (__typeof (pa4))); /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */

  memcpy (x, &a, sizeof (&a));		    /* { dg-warning "call is the same expression as the source; did you mean to remove the addressof" } */
  memcpy (x, pa1, sizeof (pa1));	    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  memcpy (x, pa2, sizeof pa2);		    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  memcpy (x, pa3, sizeof (pa3));	    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  memcpy (x, pa4, sizeof pa4);		    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  memcpy (x, pa1, sizeof (struct A *));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  memcpy (x, pa2, sizeof (PTA));    	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  memcpy (x, pa3, sizeof (PA));		    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  memcpy (x, pa4, sizeof (__typeof (pa4))); /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */

  memmove (&a, x, sizeof (&a));		    /* { dg-warning "call is the same expression as the destination; did you mean to remove the addressof" } */
  memmove (pa1, x, sizeof (pa1));	    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memmove (pa2, x, sizeof pa2);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memmove (pa3, x, sizeof (pa3));	    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memmove (pa4, x, sizeof pa4);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memmove (pa1, x, sizeof (struct A *));    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  memmove (pa2, x, sizeof (PTA));    	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  memmove (pa3, x, sizeof (PA));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  memmove (pa4, x, sizeof (__typeof (pa4)));/* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */

  memmove (x, &a, sizeof (&a));		    /* { dg-warning "call is the same expression as the source; did you mean to remove the addressof" } */
  memmove (x, pa1, sizeof (pa1));	    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  memmove (x, pa2, sizeof pa2);		    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  memmove (x, pa3, sizeof (pa3));	    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  memmove (x, pa4, sizeof pa4);		    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  memmove (x, pa1, sizeof (struct A *));    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  memmove (x, pa2, sizeof (PTA));    	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  memmove (x, pa3, sizeof (PA));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  memmove (x, pa4, sizeof (__typeof (pa4)));/* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */

  /* These are correct, no warning.  */
  memset (&a, 0, sizeof a);
  memset (&a, 0, sizeof (a));
  memset (&a, 0, sizeof (struct A));
  memset (&a, 0, sizeof (const struct A));
  memset (&a, 0, sizeof (volatile struct A));
  memset (&a, 0, sizeof (volatile const struct A));
  memset (&a, 0, sizeof (TA));
  memset (&a, 0, sizeof (__typeof (*&a)));
  memset (pa1, 0, sizeof (*pa1));
  memset (pa2, 0, sizeof (*pa3));
  memset (pa3, 0, sizeof (__typeof (*pa3)));
  /* These are probably broken, but obfuscated, no warning.  */
  memset ((void *) &a, 0, sizeof (&a));
  memset ((char *) &a, 0, sizeof (&a));
  memset (&a, 0, sizeof (&a) + 0);
  memset (&a, 0, 0 + sizeof (&a));

  /* These are correct, no warning.  */
  memcpy (&a, x, sizeof a);
  memcpy (&a, x, sizeof (a));
  memcpy (&a, x, sizeof (struct A));
  memcpy (&a, x, sizeof (const struct A));
  memcpy (&a, x, sizeof (volatile struct A));
  memcpy (&a, x, sizeof (volatile const struct A));
  memcpy (&a, x, sizeof (TA));
  memcpy (&a, x, sizeof (__typeof (*&a)));
  memcpy (pa1, x, sizeof (*pa1));
  memcpy (pa2, x, sizeof (*pa3));
  memcpy (pa3, x, sizeof (__typeof (*pa3)));
  /* These are probably broken, but obfuscated, no warning.  */
  memcpy ((void *) &a, x, sizeof (&a));
  memcpy ((char *) &a, x, sizeof (&a));
  memcpy (&a, x, sizeof (&a) + 0);
  memcpy (&a, x, 0 + sizeof (&a));

  /* These are correct, no warning.  */
  memcpy (x, &a, sizeof a);
  memcpy (x, &a, sizeof (a));
  memcpy (x, &a, sizeof (struct A));
  memcpy (x, &a, sizeof (const struct A));
  memcpy (x, &a, sizeof (volatile struct A));
  memcpy (x, &a, sizeof (volatile const struct A));
  memcpy (x, &a, sizeof (TA));
  memcpy (x, &a, sizeof (__typeof (*&a)));
  memcpy (x, pa1, sizeof (*pa1));
  memcpy (x, pa2, sizeof (*pa3));
  memcpy (x, pa3, sizeof (__typeof (*pa3)));
  /* These are probably broken, but obfuscated, no warning.  */
  memcpy (x, (void *) &a, sizeof (&a));
  memcpy (x, (char *) &a, sizeof (&a));
  memcpy (x, &a, sizeof (&a) + 0);
  memcpy (x, &a, 0 + sizeof (&a));

  /* These are correct, no warning.  */
  memmove (&a, x, sizeof a);
  memmove (&a, x, sizeof (a));
  memmove (&a, x, sizeof (struct A));
  memmove (&a, x, sizeof (const struct A));
  memmove (&a, x, sizeof (volatile struct A));
  memmove (&a, x, sizeof (volatile const struct A));
  memmove (&a, x, sizeof (TA));
  memmove (&a, x, sizeof (__typeof (*&a)));
  memmove (pa1, x, sizeof (*pa1));
  memmove (pa2, x, sizeof (*pa3));
  memmove (pa3, x, sizeof (__typeof (*pa3)));
  /* These are probably broken, but obfuscated, no warning.  */
  memmove ((void *) &a, x, sizeof (&a));
  memmove ((char *) &a, x, sizeof (&a));
  memmove (&a, x, sizeof (&a) + 0);
  memmove (&a, x, 0 + sizeof (&a));

  /* These are correct, no warning.  */
  memmove (x, &a, sizeof a);
  memmove (x, &a, sizeof (a));
  memmove (x, &a, sizeof (struct A));
  memmove (x, &a, sizeof (const struct A));
  memmove (x, &a, sizeof (volatile struct A));
  memmove (x, &a, sizeof (volatile const struct A));
  memmove (x, &a, sizeof (TA));
  memmove (x, &a, sizeof (__typeof (*&a)));
  memmove (x, pa1, sizeof (*pa1));
  memmove (x, pa2, sizeof (*pa3));
  memmove (x, pa3, sizeof (__typeof (*pa3)));
  /* These are probably broken, but obfuscated, no warning.  */
  memmove (x, (void *) &a, sizeof (&a));
  memmove (x, (char *) &a, sizeof (&a));
  memmove (x, &a, sizeof (&a) + 0);
  memmove (x, &a, 0 + sizeof (&a));
}

void
f2 (void *x)
{
  struct B b, *pb1 = &b;
  TB *pb2 = &b;
  PB pb3 = &b;
  PTB pb4 = &b;
  memset (&b, 0, sizeof (&b));		    /* { dg-warning "call is the same expression as the destination; did you mean to remove the addressof" } */
  memset (pb1, 0, sizeof (pb1));	    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memset (pb2, 0, sizeof pb2);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memset (pb3, 0, sizeof (pb3));	    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memset (pb4, 0, sizeof pb4);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memset (pb1, 0, sizeof (struct B *));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  memset (pb2, 0, sizeof (PTB));    	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  memset (pb3, 0, sizeof (PB));		    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  memset (pb4, 0, sizeof (__typeof (pb4))); /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */

  memcpy (&b, x, sizeof (&b));		    /* { dg-warning "call is the same expression as the destination; did you mean to remove the addressof" } */
  memcpy (pb1, x, sizeof (pb1));	    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memcpy (pb2, x, sizeof pb2);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memcpy (pb3, x, sizeof (pb3));	    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memcpy (pb4, x, sizeof pb4);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memcpy (pb1, x, sizeof (struct B *));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  memcpy (pb2, x, sizeof (PTB));    	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  memcpy (pb3, x, sizeof (PB));		    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  memcpy (pb4, x, sizeof (__typeof (pb4))); /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */

  memcpy (x, &b, sizeof (&b));		    /* { dg-warning "call is the same expression as the source; did you mean to remove the addressof" } */
  memcpy (x, pb1, sizeof (pb1));	    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  memcpy (x, pb2, sizeof pb2);		    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  memcpy (x, pb3, sizeof (pb3));	    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  memcpy (x, pb4, sizeof pb4);		    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  memcpy (x, pb1, sizeof (struct B *));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  memcpy (x, pb2, sizeof (PTB));    	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  memcpy (x, pb3, sizeof (PB));		    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  memcpy (x, pb4, sizeof (__typeof (pb4))); /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */

  memmove (&b, x, sizeof (&b));		    /* { dg-warning "call is the same expression as the destination; did you mean to remove the addressof" } */
  memmove (pb1, x, sizeof (pb1));	    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memmove (pb2, x, sizeof pb2);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memmove (pb3, x, sizeof (pb3));	    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memmove (pb4, x, sizeof pb4);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */
  memmove (pb1, x, sizeof (struct B *));    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  memmove (pb2, x, sizeof (PTB));    	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  memmove (pb3, x, sizeof (PB));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */
  memmove (pb4, x, sizeof (__typeof (pb4)));/* { dg-warning "call is the same pointer type \[^\n\r\]* as the destination; expected \[^\n\r\]* or an explicit length" } */

  memmove (x, &b, sizeof (&b));		    /* { dg-warning "call is the same expression as the source; did you mean to remove the addressof" } */
  memmove (x, pb1, sizeof (pb1));	    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  memmove (x, pb2, sizeof pb2);		    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  memmove (x, pb3, sizeof (pb3));	    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  memmove (x, pb4, sizeof pb4);		    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  memmove (x, pb1, sizeof (struct B *));    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  memmove (x, pb2, sizeof (PTB));    	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  memmove (x, pb3, sizeof (PB));	    /* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  memmove (x, pb4, sizeof (__typeof (pb4)));/* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */

  /* These are correct, no warning.  */
  memset (&b, 0, sizeof b);
  memset (&b, 0, sizeof (b));
  memset (&b, 0, sizeof (struct B));
  memset (&b, 0, sizeof (const struct B));
  memset (&b, 0, sizeof (volatile struct B));
  memset (&b, 0, sizeof (volatile const struct B));
  memset (&b, 0, sizeof (TB));
  memset (&b, 0, sizeof (__typeof (*&b)));
  memset (pb1, 0, sizeof (*pb1));
  memset (pb2, 0, sizeof (*pb3));
  memset (pb3, 0, sizeof (__typeof (*pb3)));
  /* These are probably broken, but obfuscated, no warning.  */
  memset ((void *) &b, 0, sizeof (&b));
  memset ((char *) &b, 0, sizeof (&b));
  memset (&b, 0, sizeof (&b) + 0);
  memset (&b, 0, 0 + sizeof (&b));

  /* These are correct, no warning.  */
  memcpy (&b, x, sizeof b);
  memcpy (&b, x, sizeof (b));
  memcpy (&b, x, sizeof (struct B));
  memcpy (&b, x, sizeof (const struct B));
  memcpy (&b, x, sizeof (volatile struct B));
  memcpy (&b, x, sizeof (volatile const struct B));
  memcpy (&b, x, sizeof (TB));
  memcpy (&b, x, sizeof (__typeof (*&b)));
  memcpy (pb1, x, sizeof (*pb1));
  memcpy (pb2, x, sizeof (*pb3));
  memcpy (pb3, x, sizeof (__typeof (*pb3)));
  /* These are probably broken, but obfuscated, no warning.  */
  memcpy ((void *) &b, x, sizeof (&b));
  memcpy ((char *) &b, x, sizeof (&b));
  memcpy (&b, x, sizeof (&b) + 0);
  memcpy (&b, x, 0 + sizeof (&b));

  /* These are correct, no warning.  */
  memcpy (x, &b, sizeof b);
  memcpy (x, &b, sizeof (b));
  memcpy (x, &b, sizeof (struct B));
  memcpy (x, &b, sizeof (const struct B));
  memcpy (x, &b, sizeof (volatile struct B));
  memcpy (x, &b, sizeof (volatile const struct B));
  memcpy (x, &b, sizeof (TB));
  memcpy (x, &b, sizeof (__typeof (*&b)));
  memcpy (x, pb1, sizeof (*pb1));
  memcpy (x, pb2, sizeof (*pb3));
  memcpy (x, pb3, sizeof (__typeof (*pb3)));
  /* These are probably broken, but obfuscated, no warning.  */
  memcpy (x, (void *) &b, sizeof (&b));
  memcpy (x, (char *) &b, sizeof (&b));
  memcpy (x, &b, sizeof (&b) + 0);
  memcpy (x, &b, 0 + sizeof (&b));

  /* These are correct, no warning.  */
  memmove (&b, x, sizeof b);
  memmove (&b, x, sizeof (b));
  memmove (&b, x, sizeof (struct B));
  memmove (&b, x, sizeof (const struct B));
  memmove (&b, x, sizeof (volatile struct B));
  memmove (&b, x, sizeof (volatile const struct B));
  memmove (&b, x, sizeof (TB));
  memmove (&b, x, sizeof (__typeof (*&b)));
  memmove (pb1, x, sizeof (*pb1));
  memmove (pb2, x, sizeof (*pb3));
  memmove (pb3, x, sizeof (__typeof (*pb3)));
  /* These are probably broken, but obfuscated, no warning.  */
  memmove ((void *) &b, x, sizeof (&b));
  memmove ((char *) &b, x, sizeof (&b));
  memmove (&b, x, sizeof (&b) + 0);
  memmove (&b, x, 0 + sizeof (&b));

  /* These are correct, no warning.  */
  memmove (x, &b, sizeof b);
  memmove (x, &b, sizeof (b));
  memmove (x, &b, sizeof (struct B));
  memmove (x, &b, sizeof (const struct B));
  memmove (x, &b, sizeof (volatile struct B));
  memmove (x, &b, sizeof (volatile const struct B));
  memmove (x, &b, sizeof (TB));
  memmove (x, &b, sizeof (__typeof (*&b)));
  memmove (x, pb1, sizeof (*pb1));
  memmove (x, pb2, sizeof (*pb3));
  memmove (x, pb3, sizeof (__typeof (*pb3)));
  /* These are probably broken, but obfuscated, no warning.  */
  memmove (x, (void *) &b, sizeof (&b));
  memmove (x, (char *) &b, sizeof (&b));
  memmove (x, &b, sizeof (&b) + 0);
  memmove (x, &b, 0 + sizeof (&b));
}

void
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
  memset (y, 0, sizeof (y));		    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */
  memset (y1, 0, sizeof (y1));		    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */
  memset (y2, 0, sizeof (y2));		    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */
  memset (&c, 0, sizeof (&c));		    /* { dg-warning "call is the same expression as the destination; did you mean to remove the addressof" } */
  memset (w, 0, sizeof w);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */

  memcpy (y, x, sizeof (y));		    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */
  memcpy (y1, x, sizeof (y1));		    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */
  memcpy (y2, x, sizeof (y2));		    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */
  memcpy (&c, x, sizeof (&c));		    /* { dg-warning "call is the same expression as the destination; did you mean to remove the addressof" } */
  memcpy (w, x, sizeof w);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */

  memcpy (x, y, sizeof (y));		    /* { dg-warning "call is the same expression as the source; did you mean to provide an explicit length" } */
  memcpy (x, y1, sizeof (y1));		    /* { dg-warning "call is the same expression as the source; did you mean to provide an explicit length" } */
  memcpy (x, y2, sizeof (y2));		    /* { dg-warning "call is the same expression as the source; did you mean to provide an explicit length" } */
  memcpy (x, &c, sizeof (&c));		    /* { dg-warning "call is the same expression as the source; did you mean to remove the addressof" } */
  memcpy (x, w, sizeof w);		    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */

  memmove (y, x, sizeof (y));		    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */
  memmove (y1, x, sizeof (y1));		    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */
  memmove (y2, x, sizeof (y2));		    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */
  memmove (&c, x, sizeof (&c));		    /* { dg-warning "call is the same expression as the destination; did you mean to remove the addressof" } */
  memmove (w, x, sizeof w);		    /* { dg-warning "call is the same expression as the destination; did you mean to dereference it" } */

  memmove (x, y, sizeof (y));		    /* { dg-warning "call is the same expression as the source; did you mean to provide an explicit length" } */
  memmove (x, y1, sizeof (y1));		    /* { dg-warning "call is the same expression as the source; did you mean to provide an explicit length" } */
  memmove (x, y2, sizeof (y2));		    /* { dg-warning "call is the same expression as the source; did you mean to provide an explicit length" } */
  memmove (x, &c, sizeof (&c));		    /* { dg-warning "call is the same expression as the source; did you mean to remove the addressof" } */
  memmove (x, w, sizeof w);		    /* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */

  /* These are correct, no warning.  */
  memset (y, 0, sizeof (*y));
  memset (y1, 0, sizeof (*y2));
  memset (buf1, 0, sizeof buf1);
  memset (buf3, 0, sizeof (buf3));
  memset (&buf3[0], 0, sizeof (buf3));
  memset (&buf4[0], 0, sizeof (buf4));
  memset (w, 0, sizeof (X));
  /* These are probably broken, but obfuscated, no warning.  */
  memset ((void *) y, 0, sizeof (y));
  memset ((char *) y1, 0, sizeof (y2));
  memset (y, 0, sizeof (y) + 0);
  memset (y1, 0, 0 + sizeof (y2));
  memset ((void *) &c, 0, sizeof (&c));
  memset ((signed char *) &c, 0, sizeof (&c));
  memset (&c, 0, sizeof (&c) + 0);
  memset (&c, 0, 0 + sizeof (&c));

  /* These are correct, no warning.  */
  memcpy (y, x, sizeof (*y));
  memcpy (y1, x, sizeof (*y2));
  memcpy (buf1, x, sizeof buf1);
  memcpy (buf3, x, sizeof (buf3));
  memcpy (&buf3[0], x, sizeof (buf3));
  memcpy (&buf4[0], x, sizeof (buf4));
  memcpy (&y3, y, sizeof (y3));
  memcpy ((char *) &y3, y, sizeof (y3));
  memcpy (w, x, sizeof (X));
  /* These are probably broken, but obfuscated, no warning.  */
  memcpy ((void *) y, x, sizeof (y));
  memcpy ((char *) y1, x, sizeof (y2));
  memcpy (y, x, sizeof (y) + 0);
  memcpy (y1, x, 0 + sizeof (y2));
  memcpy ((void *) &c, x, sizeof (&c));
  memcpy ((signed char *) &c, x, sizeof (&c));
  memcpy (&c, x, sizeof (&c) + 0);
  memcpy (&c, x, 0 + sizeof (&c));

  /* These are correct, no warning.  */
  memcpy (x, y, sizeof (*y));
  memcpy (x, y1, sizeof (*y2));
  memcpy (x, buf1, sizeof buf1);
  memcpy (x, buf3, sizeof (buf3));
  memcpy (x, &buf3[0], sizeof (buf3));
  memcpy (x, &buf4[0], sizeof (buf4));
  memcpy (y, &y3, sizeof (y3));
  memcpy (y, (char *) &y3, sizeof (y3));
  memcpy (x, w, sizeof (X));
  /* These are probably broken, but obfuscated, no warning.  */
  memcpy (x, (void *) y, sizeof (y));
  memcpy (x, (char *) y1, sizeof (y2));
  memcpy (x, y, sizeof (y) + 0);
  memcpy (x, y1, 0 + sizeof (y2));
  memcpy (x, (void *) &c, sizeof (&c));
  memcpy (x, (signed char *) &c, sizeof (&c));
  memcpy (x, &c, sizeof (&c) + 0);
  memcpy (x, &c, 0 + sizeof (&c));

  /* These are correct, no warning.  */
  memmove (y, x, sizeof (*y));
  memmove (y1, x, sizeof (*y2));
  memmove (buf1, x, sizeof buf1);
  memmove (buf3, x, sizeof (buf3));
  memmove (&buf3[0], x, sizeof (buf3));
  memmove (&buf4[0], x, sizeof (buf4));
  memmove (&y3, y, sizeof (y3));
  memmove ((char *) &y3, y, sizeof (y3));
  memmove (w, x, sizeof (X));
  /* These are probably broken, but obfuscated, no warning.  */
  memmove ((void *) y, x, sizeof (y));
  memmove ((char *) y1, x, sizeof (y2));
  memmove (y, x, sizeof (y) + 0);
  memmove (y1, x, 0 + sizeof (y2));
  memmove ((void *) &c, x, sizeof (&c));
  memmove ((signed char *) &c, x, sizeof (&c));
  memmove (&c, x, sizeof (&c) + 0);
  memmove (&c, x, 0 + sizeof (&c));

  /* These are correct, no warning.  */
  memmove (x, y, sizeof (*y));
  memmove (x, y1, sizeof (*y2));
  memmove (x, buf1, sizeof buf1);
  memmove (x, buf3, sizeof (buf3));
  memmove (x, &buf3[0], sizeof (buf3));
  memmove (x, &buf4[0], sizeof (buf4));
  memmove (y, &y3, sizeof (y3));
  memmove (y, (char *) &y3, sizeof (y3));
  memmove (x, w, sizeof (X));
  /* These are probably broken, but obfuscated, no warning.  */
  memmove (x, (void *) y, sizeof (y));
  memmove (x, (char *) y1, sizeof (y2));
  memmove (x, y, sizeof (y) + 0);
  memmove (x, y1, 0 + sizeof (y2));
  memmove (x, (void *) &c, sizeof (&c));
  memmove (x, (signed char *) &c, sizeof (&c));
  memmove (x, &c, sizeof (&c) + 0);
  memmove (x, &c, 0 + sizeof (&c));
}

void
f4 (char *x, char **y, int z, char w[64])
{
  const char *s1 = "foobarbaz";
  const char *s2 = "abcde12345678";
  strncpy (x, s1, sizeof (s1));		    /* { dg-warning "call is the same expression as the source; did you mean to provide an explicit length" } */
  strncat (x, s2, sizeof (s2));		    /* { dg-warning "call is the same expression as the source; did you mean to provide an explicit length" } */
  stpncpy (x, s1, sizeof (s1));		    /* { dg-warning "call is the same expression as the source; did you mean to provide an explicit length" } */

  strncpy (w, s1, sizeof (w));		    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */
  strncat (w, s2, sizeof (w));		    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */
  stpncpy (w, s1, sizeof (w));		    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */

  /* These are pointless when the destination is large enough, and
     cause overflow otherwise.  If the copies are guaranteed to be
     safe the calls might as well be replaced by strcat(), strcpy(),
     or memcpy().  */
  const char s3[] = "foobarbaz";
  const char s4[] = "abcde12345678";
  strncpy (x, s3, sizeof (s3));             /* { dg-warning "call is the same expression as the source; did you mean to use the size of the destination?" } */
  strncat (x, s4, sizeof (s4));             /* { dg-warning "call is the same expression as the source; did you mean to use the size of the destination?" } */
  stpncpy (x, s3, sizeof (s3));             /* { dg-warning "call is the same expression as the source; did you mean to use the size of the destination?" } */
}

/* { dg-prune-output "\[\n\r\]*writing\[\n\r\]*" } */
/* { dg-prune-output "\[\n\r\]*reading\[\n\r\]*" } */
