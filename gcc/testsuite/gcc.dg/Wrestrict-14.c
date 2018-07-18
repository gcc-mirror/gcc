/* PR tree-optimization/83456 - -Wrestrict false positive on a non-overlapping
   memcpy in an inline function
   Verify that calls to built-in functions are diagnosed when the pointer
   arguments to their restrict-qualified parameters are the same (the absence
   of the false positives reported in PR 83456 is tested in Wrestrict-12.c.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wrestrict -Wno-stringop-truncation" }  */

typedef __SIZE_TYPE__ size_t;

extern void* memcpy (void* restrict, const void* restrict, size_t);
extern void* mempcpy (void* restrict, const void* restrict, size_t);
extern char* stpncpy (char* restrict, const char* restrict, size_t);
extern char* stpcpy (char* restrict, const char* restrict);
extern char* strncat (char* restrict, const char* restrict, size_t);
extern char* strcat (char* restrict, const char* restrict);
extern char* strncpy (char* restrict, const char* restrict, size_t);
extern char* strcpy (char* restrict, const char* restrict);

struct S
{
  char a[4];
  char *p;
} s;

void sink (void*);

void test_memcpy (char *p, struct S *q, size_t n)
{
  /* The behavior of memcpy() is undefined only when when copying takes
     place between overlapping objects.  Since a call with a size of zero
     does nothing, it should not be diagnosed.  */
  memcpy (p, p, 0);
  sink (p);

  memcpy (p, p, 1);               /* { dg-warning "\\\[-Wrestrict]" } */
  sink (p);

  memcpy (p, p, n);               /* { dg-warning "\\\[-Wrestrict]" } */
  sink (p);

  memcpy (q->a, q->a, 0);
  sink (q);

  memcpy (q->p, q->p, 1);         /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);

  memcpy (&q->a[0], q->a, n);     /* { dg-warning "\\\[-Wrestrict]" "bug ????" { xfail *-*-* } } */
  sink (q);

  memcpy (q, q->a, n);            /* { dg-warning "\\\[-Wrestrict]" "bug ????" { xfail *-*-* } } */
  sink (q);
}

void test_mempcpy (char *p, struct S *q, size_t n)
{
  mempcpy (p, p, 0);
  sink (p);

  mempcpy (p, p, 1);              /* { dg-warning "\\\[-Wrestrict]" } */
  sink (p);

  mempcpy (p, p, n);              /* { dg-warning "\\\[-Wrestrict]" } */
  sink (p);

  mempcpy (q->a, q->a, 0);
  sink (q);

  mempcpy (q->p, q->p, 1);        /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);

  mempcpy (&q->a[0], q->a, n);    /* { dg-warning "\\\[-Wrestrict]" "bug ????" { xfail *-*-* } } */
  sink (q);

  mempcpy (q, q->a, n);           /* { dg-warning "\\\[-Wrestrict]" "bug ????" { xfail *-*-* } } */
  sink (q);
}

void test_strncat (char *p, struct S *q, size_t n)
{
  strncat (p, p, 0);
  sink (p);

  strncat (p, p, 1);              /* { dg-warning "\\\[-Wrestrict]" } */
  sink (p);

  strncat (p, p, n);              /* { dg-warning "\\\[-Wrestrict]" } */
  sink (p);

  strncat (q->a, q->a, n);        /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);

  strncat (&q->a[0], &q->a[0], n);/* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);

  strncat (q->a, &q->a[0], n);    /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);

  strncat (q->p, &q->p[0], n);    /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);
}

void test_strcat (char *p, struct S *q, size_t n)
{
  strcat (p, p);                  /* { dg-warning "\\\[-Wrestrict]" } */
  sink (p);

  strcat (p, p);                  /* { dg-warning "\\\[-Wrestrict]" } */
  sink (p);

  strcat (p, p);                  /* { dg-warning "\\\[-Wrestrict]" } */
  sink (p);

  strcat (q->a, q->a);            /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);

  strcat (&q->a[0], &q->a[0]);    /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);

  strcat (q->a, &q->a[0]);        /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);

  strcat (q->p, &q->p[0]);        /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);
}

void test_stpncpy (char *p, struct S *q, size_t n)
{
  stpncpy (p, p, 0);              /* { dg-warning "\\\[-Wrestrict]" } */
  sink (p);

  stpncpy (p, p, 1);              /* { dg-warning "\\\[-Wrestrict]" } */
  sink (p);

  stpncpy (p, p, n);              /* { dg-warning "\\\[-Wrestrict]" } */
  sink (p);

  stpncpy (q->a, q->a, n);        /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);

  stpncpy (&q->a[0], &q->a[0], n);/* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);

  stpncpy (q->a, &q->a[0], n);    /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);

  stpncpy (q->p, &q->p[0], n);    /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);
}

void test_stpcpy (char *p, struct S *q, size_t n)
{
  stpcpy (p, p);                  /* { dg-warning "\\\[-Wrestrict]" } */
  sink (p);

  stpcpy (p, p);                  /* { dg-warning "\\\[-Wrestrict]" } */
  sink (p);

  stpcpy (p, p);                  /* { dg-warning "\\\[-Wrestrict]" } */
  sink (p);

  stpcpy (q->a, q->a);            /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);

  stpcpy (&q->a[0], &q->a[0]);    /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);

  stpcpy (q->a, &q->a[0]);        /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);

  stpcpy (q->p, &q->p[0]);        /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);
}

void test_strncpy (char *p, struct S *q, size_t n)
{
  strncpy (p, p, 0);
  sink (p);

  strncpy (p, p, 1);              /* { dg-warning "\\\[-Wrestrict]" } */
  sink (p);

  strncpy (p, p, n);              /* { dg-warning "\\\[-Wrestrict]" } */
  sink (p);

  strncpy (q->a, q->a, n);        /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);

  strncpy (&q->a[0], &q->a[0], n);/* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);

  strncpy (q->a, &q->a[0], n);    /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);

  strncpy (q->p, &q->p[0], n);    /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);
}

void test_strcpy (char *p, struct S *q, size_t n)
{
  strcpy (p, p);                  /* { dg-warning "\\\[-Wrestrict]" } */
  sink (p);

  strcpy (p, p);                  /* { dg-warning "\\\[-Wrestrict]" } */
  sink (p);

  strcpy (p, p);                  /* { dg-warning "\\\[-Wrestrict]" } */
  sink (p);

  strcpy (q->a, q->a);            /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);

  strcpy (&q->a[0], &q->a[0]);    /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);

  strcpy (q->a, &q->a[0]);        /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);

  strcpy (q->p, &q->p[0]);        /* { dg-warning "\\\[-Wrestrict]" } */
  sink (q);
}
