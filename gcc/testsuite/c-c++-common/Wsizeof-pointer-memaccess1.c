/* Test -Wsizeof-pointer-memaccess warnings.  */
/* { dg-do compile } */
/* { dg-options "-Wall -Wno-array-bounds -Wno-sizeof-array-argument -Wno-stringop-overflow -Wno-stringop-overread" } */
/* { dg-options "-Wall -Wno-array-bounds -Wno-sizeof-array-argument -Wno-c++-compat -Wno-stringop-overflow -Wno-stringop-overread" { target c } } */

typedef __SIZE_TYPE__ size_t;
#ifdef __cplusplus
extern "C" {
#endif
extern int snprintf (char *, size_t, const char *, ...);
extern int vsnprintf (char *, size_t, const char *, __builtin_va_list);
extern void *memchr (const void *, int, size_t);
#ifdef __cplusplus
}
#endif

struct A { short a, b; int c, d; long e, f; };
typedef struct A TA;
typedef struct A *PA;
typedef TA *PTA;
struct B {};
typedef struct B TB;
typedef struct B *PB;
typedef TB *PTB;
typedef int X[3][3][3];

void foo (void **);

void
f1 (void *x)
{
  struct A a, *pa1 = &a;
  TA *pa2 = &a;
  PA pa3 = &a;
  PTA pa4 = &a;
  void *arr[100];
  int i = 0;
  arr[i++] = memchr (&a, 0, sizeof (&a));		/* { dg-warning "call is the same expression as the source; did you mean to remove the addressof" } */
  arr[i++] = memchr (pa1, 0, sizeof (pa1));		/* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  arr[i++] = memchr (pa2, 0, sizeof pa2);		/* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  arr[i++] = memchr (pa3, 0, sizeof (pa3));		/* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  arr[i++] = memchr (pa4, 0, sizeof pa4);		/* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  arr[i++] = memchr (pa1, 0, sizeof (struct A *));	/* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  arr[i++] = memchr (pa2, 0, sizeof (PTA));		/* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  arr[i++] = memchr (pa3, 0, sizeof (PA));		/* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  arr[i++] = memchr (pa4, 0, sizeof (__typeof (pa4)));	/* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */

  /* These are correct, no warning.  */
  arr[i++] = memchr (&a, 0, sizeof a);
  arr[i++] = memchr (&a, 0, sizeof (a));
  arr[i++] = memchr (&a, 0, sizeof (struct A));
  arr[i++] = memchr (&a, 0, sizeof (const struct A));
  arr[i++] = memchr (&a, 0, sizeof (volatile struct A));
  arr[i++] = memchr (&a, 0, sizeof (volatile const struct A));
  arr[i++] = memchr (&a, 0, sizeof (TA));
  arr[i++] = memchr (&a, 0, sizeof (__typeof (*&a)));
  arr[i++] = memchr (pa1, 0, sizeof (*pa1));
  arr[i++] = memchr (pa2, 0, sizeof (*pa3));
  arr[i++] = memchr (pa3, 0, sizeof (__typeof (*pa3)));
  /* These are probably broken, but obfuscated, no warning.  */
  arr[i++] = memchr ((void *) &a, 0, sizeof (&a));
  arr[i++] = memchr ((char *) &a, 0, sizeof (&a));
  arr[i++] = memchr (&a, 0, sizeof (&a) + 0);
  arr[i++] = memchr (&a, 0, 0 + sizeof (&a));

  foo (arr);
}

void
f2 (void *x)
{
  struct B b, *pb1 = &b;
  TB *pb2 = &b;
  PB pb3 = &b;
  PTB pb4 = &b;
  void *arr[100];
  int i = 0;
  arr[i++] = memchr (&b, 0, sizeof (&b));		/* { dg-warning "call is the same expression as the source; did you mean to remove the addressof" } */
  arr[i++] = memchr (pb1, 0, sizeof (pb1));		/* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  arr[i++] = memchr (pb2, 0, sizeof pb2);		/* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  arr[i++] = memchr (pb3, 0, sizeof (pb3));		/* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  arr[i++] = memchr (pb4, 0, sizeof pb4);		/* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */
  arr[i++] = memchr (pb1, 0, sizeof (struct B *));	/* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  arr[i++] = memchr (pb2, 0, sizeof (PTB));		/* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  arr[i++] = memchr (pb3, 0, sizeof (PB));		/* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */
  arr[i++] = memchr (pb4, 0, sizeof (__typeof (pb4)));	/* { dg-warning "call is the same pointer type \[^\n\r\]* as the source; expected \[^\n\r\]* or an explicit length" } */

  /* These are correct, no warning.  */
  arr[i++] = memchr (&b, 0, sizeof b);
  arr[i++] = memchr (&b, 0, sizeof (b));
  arr[i++] = memchr (&b, 0, sizeof (struct B));
  arr[i++] = memchr (&b, 0, sizeof (const struct B));
  arr[i++] = memchr (&b, 0, sizeof (volatile struct B));
  arr[i++] = memchr (&b, 0, sizeof (volatile const struct B));
  arr[i++] = memchr (&b, 0, sizeof (TB));
  arr[i++] = memchr (&b, 0, sizeof (__typeof (*&b)));
  arr[i++] = memchr (pb1, 0, sizeof (*pb1));
  arr[i++] = memchr (pb2, 0, sizeof (*pb3));
  arr[i++] = memchr (pb3, 0, sizeof (__typeof (*pb3)));
  /* These are probably broken, but obfuscated, no warning.  */
  arr[i++] = memchr ((void *) &b, 0, sizeof (&b));
  arr[i++] = memchr ((char *) &b, 0, sizeof (&b));
  arr[i++] = memchr (&b, 0, sizeof (&b) + 0);
  arr[i++] = memchr (&b, 0, 0 + sizeof (&b));

  foo (arr);
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
  void *arr[100];
  int i = 0;
  arr[i++] = memchr (y, 0, sizeof (y));			/* { dg-warning "call is the same expression as the source; did you mean to provide an explicit length" } */
  arr[i++] = memchr (y1, 0, sizeof (y1));		/* { dg-warning "call is the same expression as the source; did you mean to provide an explicit length" } */
  arr[i++] = memchr (y2, 0, sizeof (y2));		/* { dg-warning "call is the same expression as the source; did you mean to provide an explicit length" } */
  arr[i++] = memchr (&c, 0, sizeof (&c));		/* { dg-warning "call is the same expression as the source; did you mean to remove the addressof" } */
  arr[i++] = memchr (w, 0, sizeof w);			/* { dg-warning "call is the same expression as the source; did you mean to dereference it" } */

  /* These are correct, no warning.  */
  arr[i++] = memchr (y, 0, sizeof (*y));
  arr[i++] = memchr (y1, 0, sizeof (*y2));
  arr[i++] = memchr (buf1, 0, sizeof buf1);
  arr[i++] = memchr (buf3, 0, sizeof (buf3));
  arr[i++] = memchr (&buf3[0], 0, sizeof (buf3));
  arr[i++] = memchr (&buf4[0], 0, sizeof (buf4));
  arr[i++] = memchr (w, 0, sizeof (X));
  /* These are probably broken, but obfuscated, no warning.  */
  arr[i++] = memchr ((void *) y, 0, sizeof (y));
  arr[i++] = memchr ((char *) y1, 0, sizeof (y2));
  arr[i++] = memchr (y, 0, sizeof (y) + 0);
  arr[i++] = memchr (y1, 0, 0 + sizeof (y2));
  arr[i++] = memchr ((void *) &c, 0, sizeof (&c));
  arr[i++] = memchr ((signed char *) &c, 0, sizeof (&c));
  arr[i++] = memchr (&c, 0, sizeof (&c) + 0);
  arr[i++] = memchr (&c, 0, 0 + sizeof (&c));

  foo (arr);
}

void
f4 (char x[64], char *y, __builtin_va_list ap)
{
  char buf[128], *p = buf;
  snprintf (x, sizeof (x), "%s", y);	    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */
  vsnprintf (x, sizeof (x), "%s", ap);	    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */
  snprintf (p, sizeof (p), "%s", y);	    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */
  vsnprintf (p, sizeof (p), "%s", ap);	    /* { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length" } */

  /* These are correct, no warning.  */
  snprintf (buf, sizeof (buf), "%s", y);
  vsnprintf (buf, sizeof (buf), "%s", ap);
  snprintf (p, sizeof (buf), "%s", y);
  vsnprintf (p, sizeof (buf), "%s", ap);
}

/* { dg-prune-output "-Wuninitialized" } */
