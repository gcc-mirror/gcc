/* PR ????? - No warning on attempts to access free object
   Verify that freeing unallocated objects referenced either directly
   or through pointers is diagnosed.  In most cases this doesn't require
   optimization.
   { dg-do compile }
   { dg-options "-Wall -Wfree-nonheap-object" }  */

typedef __INTPTR_TYPE__ intptr_t;
typedef __SIZE_TYPE__   size_t;

extern void free (void*);
extern void* malloc (size_t);
extern void* realloc (void *p, size_t);

void sink (void*, ...);

extern char ecarr[];
extern void* eparr[];

extern char *eptr;

void* source (void);

void nowarn_free (void *p, void **pp, size_t n, intptr_t iptr)
{
  free (p);

  p = 0;
  free (p);

  p = malloc (n);
  sink (p);
  free (p);

  p = malloc (n);
  sink (p);

  p = realloc (p, n * 2);
  sink (p);
  free (p);

  free ((void*)iptr);

  p = source ();
  free (p);

  p = source ();
  p = (char*)p - 1;
  free (p);

  free (*pp);
}

void warn_free_extern_arr (void)
{
  free (ecarr);               // { dg-warning "\\\[-Wfree-nonheap-object" }
}

void warn_free_extern_arr_offset (int i)
{
  char *p = ecarr + i;
  free (p);                   // { dg-warning "\\\[-Wfree-nonheap-object" }
}


void warn_free_cstint (void)
{
  void *p = (void*)1;
  sink (p);
  free (p);                   // { dg-warning "\\\[-Wfree-nonheap-object" }
}


void warn_free_func (void)
{
  void *p = warn_free_func;
  sink (p);
  free (p);                   // { dg-warning "\\\[-Wfree-nonheap-object" }
}


void warn_free_string (int i)
{
  {
    char *p = "123";
    sink (p);
    free (p);                 // { dg-warning "\\\[-Wfree-nonheap-object" }
  }
  {
    char *p = "234" + 1;
    sink (p);
    free (p);                 // { dg-warning "\\\[-Wfree-nonheap-object" }
  }
  {
    char *p = "345" + i;
    sink (p);
    free (p);                 // { dg-warning "\\\[-Wfree-nonheap-object" }
  }

  if (i >= 0)
    {
      char *p = "456" + i;
      sink (p);
      free (p);               // { dg-warning "\\\[-Wfree-nonheap-object" }
    }
}

void warn_free_local_arr (int i)
{
  {
    char a[4];
    sink (a);
    free (a);                 // { dg-warning "\\\[-Wfree-nonheap-object" }
  }
  {
    char b[5];
    sink (b);

    char *p = b + 1;
    free (p);                 // { dg-warning "\\\[-Wfree-nonheap-object" }
  }
  {
    char c[6];
    sink (c);

    char *p = c + i;
    free (p);                 // { dg-warning "\\\[-Wfree-nonheap-object" }
  }
}


void warn_free_vla (int n, int i)
{
  {
    int vla[n], *p = vla;
    sink (p);
    free (p);                 // { dg-warning "\\\[-Wfree-nonheap-object" }
  }

  {
    int vla[n + 1], *p = vla + 1;
    sink (p);
    free (p);                 // { dg-warning "\\\[-Wfree-nonheap-object" }
  }
  {
    int vla[n + 2], *p = vla + i;
    sink (p);
    free (p);                 // { dg-warning "\\\[-Wfree-nonheap-object" }
  }
}


void nowarn_free_extern_ptrarr (void)
{
  free (*eparr);
}

void nowarn_free_extern_ptrarr_offset (int i)
{
  char *p = eparr[i];
  free (p);
}


void warn_free_extern_ptrarr (void)
{
  free (eparr);               // { dg-warning "\\\[-Wfree-nonheap-object" }
}

void warn_free_extern_ptrarr_offset (int i)
{
  void *p = &eparr[i];
  free (p);                   // { dg-warning "\\\[-Wfree-nonheap-object" }
}


void nowarn_free_local_ptrarr (int i)
{
  void* a[4];
  sink (a);
  free (a[0]);
  free (a[1]);
  free (a[i]);
}


void nowarn_free_extern_ptr (void)
{
  free (eptr);
}

void nowarn_free_extern_ptr_offset (int i)
{
  char *p = eptr + i;
  free (p);
}

void nowarn_free_parm_offset (char *p, int i)
{
  char *q = p + i;
  free (q);
}

void nowarn_free_parm_neg_offset (char *p, int i)
{
  if (i >= 0)
    i = -1;

  char *q = p + i;
  free (q);
}

struct Members
{
  char a[4], *p, *q;
};

extern struct Members em;

void nowarn_free_member_ptr (struct Members *pm, int i)
{
  char *p = em.p;
  free (p);
  p = em.q + i;
  free (p);

  free (pm->q);
  p = pm->p;
  free (pm);
  free (p);
}

void nowarn_free_struct_cast (intptr_t *p)
{
  struct Members *q = (struct Members*)*p;
  if (q->p == 0)
    free (q);                 // { dg-bogus "\\\[-Wfree-nonheap-object" }
}


void warn_free_member_array (void)
{
  char *p = em.a;
  free (p);                   // { dg-warning "\\\[-Wfree-nonheap-object" }
}

void warn_free_member_array_off (int i)
{
  char *p = em.a + i;
  free (p);                   // { dg-warning "\\\[-Wfree-nonheap-object" }
}


// Range information requires optimization.
#pragma GCC optimize "1"

void warn_free_extern_ptr_pos_offset (int i)
{
  if (i <= 0)
    i = 1;

  char *q = eptr + i;
  free (q);                   // { dg-warning "\\\[-Wfree-nonheap-object" }
}

void warn_free_parm_pos_offset (char *p, int i)
{
  if (i <= 0)
    i = 1;

  char *q = p + i;
  free (q);                   // { dg-warning "\\\[-Wfree-nonheap-object" }
}
