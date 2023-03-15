/* Verify -Wuse-after-free=2 triggers for conditional as well as
   unconditional uses but not for equality expressions.  Same as
   -Wuse-after-free-5.c but with optimization.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wuse-after-free=2" } */


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

void warn_cond_double_free (void *p, int c)
{
  free (p);
  if (c)
    free (p);       // { dg-warning "pointer 'p' may be used" }
}

void warn_call_after_free (void *p)
{
  free (p);
  sink (p);         // { dg-warning "pointer 'p' used" }
}

void warn_cond_call_after_free (void *p, int c)
{
  free (p);
  if (c)
    sink (p);       // { dg-warning "pointer 'p' may be used" }
}

void* warn_return_after_free (void *p)
{
  free (p);
  return p;         // { dg-warning "pointer 'p' used" }
}

void* warn_cond_return_after_free (void *p, int c)
{
  free (p);
  // PHI handling not fully implemented.
  if (c)
    return p;       // { dg-warning "pointer 'p' may be used" }
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

void warn_cond_relational_after_free (char *p, char *q[], int c)
{
  free (p);

  int a[] =
    {
     c ? p < q[0] : q[0][0],  // { dg-warning "pointer 'p' may be used" }
     c ? p <= q[1] : q[1][1], // { dg-warning "pointer 'p' may be used" }
     c ? p > q[2] : q[2][2],  // { dg-warning "pointer 'p' may be used" }
     c ? p >= q[3] : q[3][3], // { dg-warning "pointer 'p' may be used" }
     c ? p == q[4] : q[4][4],
     c ? p != q[5] : q[5][5],
    };

  sink (a);
}


// Verify warning for the example in the manual.

struct A { int refcount; void *data; };

void release (struct A *p)
{
  int refcount = --p->refcount;
  free (p);
  if (refcount == 0)
    free (p->data); // { dg-warning "pointer 'p' may be used" }
}
