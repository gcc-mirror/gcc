/* Verify -Wuse-after-free=1 triggers only for unconditional uses and
   not for equality expressions.
   { dg-do compile }
   { dg-options "-O0 -Wall -Wuse-after-free=1" } */

#if __cplusplus
#  define EXTERN_C extern "C"
#else
#  define EXTERN_C extern
#endif

EXTERN_C void free (void*);

void sink (void*);


void warn_double_free (void *p)
{
  free (p);
  free (p);         // { dg-warning "pointer 'p' used" }
}

void nowarn_cond_double_free (void *p, int c)
{
  free (p);
  if (c)
    free (p);
}

void warn_call_after_free (void *p)
{
  free (p);
  sink (p);         // { dg-warning "pointer 'p' used" }
}

void nowarn_cond_call_after_free (void *p, int c)
{
  free (p);
  if (c)
    sink (p);
}

void* warn_return_after_free (void *p)
{
  free (p);
  return p;         // { dg-warning "pointer 'p' used" }
}

void* nowarn_cond_return_after_free (void *p, int c)
{
  free (p);
  if (c)
    return p;
  return 0;
}

void warn_relational_after_free (char *p, char *q[])
{
  free (p);

  int a[] =
    {
     p < q[0],      // { dg-warning "pointer 'p' used" }
     p <= q[1],     // { dg-warning "pointer 'p' used" }
     p > q[2],      // { dg-warning "pointer 'p' used" }
     p >= q[3],     // { dg-warning "pointer 'p' used" }
     p == q[4],
     p != q[5]
    };

  sink (a);
}

void nowarn_cond_relational_after_free (char *p, char *q[], int c)
{
  free (p);

  int a[] =
    {
     c ? p < q[0] : q[0][0],
     c ? p <= q[1] : q[1][1],
     c ? p > q[2] : q[2][2],
     c ? p >= q[3] : q[3][3],
     c ? p == q[4] : q[4][4],
     c ? p != q[5] : q[5][5],
    };

  sink (a);
}


// Verify no warning for the example in the manual.

struct A { int refcount; void *data; };

void release (struct A *p)
{
  int refcount = --p->refcount;
  free (p);
  if (refcount == 0)
    free (p->data);   // no warning at level 1
}
