/* PR ????? - No warning on attempts to access free object
   Verify that attempting to reallocate unallocated objects referenced
   either directly or through pointers is diagnosed.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wfree-nonheap-object" } */

typedef __SIZE_TYPE__ size_t;

extern void free (void*);
extern void* alloca (size_t);
extern void* realloc (void*, size_t);

void sink (void*, ...);

extern void* eparr[];
extern char *eptr;

extern size_t n;


void nowarn_realloc (void *p, size_t n)
{
  char *q = realloc (p, n);
  sink (q);

  q = realloc (0, n);
  sink (q);

  q = realloc (q, n * 2);
  sink (q);
}

/* Verify that calling realloc on a pointer to an unknown object minus
   some nonzero offset isn't diagnosed, but a pointer plus a positive
   offset is (a positive offset cannot point at the beginning).  */

void test_realloc_offset (char *p1, char *p2, char *p3, size_t n, int i)
{
  char *q;
  q = realloc (p1 - 1, n);
  sink (q);

  q = realloc (p2 + 1, n);    // { dg-warning "'realloc' called on pointer 'p2' with nonzero offset 1" }
  sink (q);

  q = realloc (p3 + i, n);
  sink (q);
}

void warn_realloc_extern_arr (void)
{
  extern char ecarr[];        // { gg-message "declared here" }
  char *p = ecarr;
  char *q = realloc (p, n);   // { dg-warning "'realloc' called on unallocated object 'ecarr'" }
  sink (q);
}

void warn_realloc_extern_arr_offset (int i)
{
  extern char ecarr[];
  char *p = ecarr + i;
  char *q = realloc (p, n);   // { dg-warning "\\\[-Wfree-nonheap-object" }
  sink (q);
}


void warn_realloc_string (int i)
{
  char *p, *q;
  {
    p = "123";
    sink (p);
    q = realloc (p, n);       // { dg-warning "\\\[-Wfree-nonheap-object" }
    sink (q);
  }
  {
    p = "234" + 1;
    sink (p);
    q = realloc (p, n);       // { dg-warning "\\\[-Wfree-nonheap-object" }
    sink (q);
  }
  {
    p = "123" + i;
    sink (p);
    q = realloc (p, n);       // { dg-warning "\\\[-Wfree-nonheap-object" }
    sink (q);
  }
}


void warn_realloc_alloca (int n, int i)
{
  char *p, *q;
  {
    p = alloca (n);
    sink (p);
    q = realloc (p, n);       // { dg-warning "\\\[-Wfree-nonheap-object" }
    sink (q);
  }
  {
    p = (char*)alloca (n + 1);
    sink (p);
    q = realloc (p, n);       // { dg-warning "\\\[-Wfree-nonheap-object" }
    sink (q);
  }
  {
    p = (char*)alloca (n + 2) + i;
    sink (p);
    q = realloc (p, n);       // { dg-warning "\\\[-Wfree-nonheap-object" }
    sink (q);
  }
}


void warn_realloc_local_arr (int i)
{
  char *q;
  {
    char a[4];
    sink (a);
    q = realloc (a, n);       // { dg-warning "\\\[-Wfree-nonheap-object" }
    sink (q);
  }

  {
    char b[5];
    sink (b);
    q = realloc (b + 1, n);   // { dg-warning "\\\[-Wfree-nonheap-object" }
    sink (q);
  }

  {
    char c[6];
    sink (c);
    q = realloc (&c[2], n);   // { dg-warning "\\\[-Wfree-nonheap-object" }
    sink (q);
  }

  {
    char d[7];
    sink (d);
    q = realloc (&d[i], n);   // { dg-warning "\\\[-Wfree-nonheap-object" }
    sink (q);
  }
}

void warn_realloc_vla (int n1, int n2, int i)
{
  char *q;
  {
    char vla[n1];
    sink (vla);
    q = realloc (vla, n2);    // { dg-warning "\\\[-Wfree-nonheap-object" }
    sink (q);
  }

  {
    char vlb[n1 + 1];
    sink (vlb);
    q = realloc (vlb + 1, n2);// { dg-warning "\\\[-Wfree-nonheap-object" }
    sink (q);
  }

  {
    char vlc[n1 + 2];
    sink (vlc);
    q = realloc (&vlc[2], n2);// { dg-warning "\\\[-Wfree-nonheap-object" }
    sink (q);
  }

  {
    char vld[7];
    sink (vld);
    q = realloc (&vld[i], n2);// { dg-warning "\\\[-Wfree-nonheap-object" }
    sink (q);
  }
}

void nowarn_realloc_extern_ptrarr (void)
{
  char *q = realloc (*eparr, n);
  sink (q);
}

void nowarn_realloc_extern_ptrarr_offset (int i)
{
  char *p = eparr[i];
  char *q = realloc (p, n);
  sink (q);
}


void warn_realloc_extern_ptrarr (void)
{
  char *q = realloc (eparr, n);  // { dg-warning "\\\[-Wfree-nonheap-object" }
  sink (q);
}

void warn_realloc_extern_ptrarr_offset (int i)
{
  void *p = eparr + i;
  void *q = realloc (p, n);   // { dg-warning "\\\[-Wfree-nonheap-object" }
  sink (q);
}


void nowarn_realloc_extern_ptr (void)
{
  char *q = realloc (eptr, n);
  sink (q);
}

void nowarn_realloc_extern_ptr_offset (int i)
{
  char *p = eptr + i;
  char *q = realloc (p, n);
  sink (q);
}


void warn_realloc_extern_ptr_pos_offset (int i)
{
  if (i <= 0)
    i = 1;

  char *p = eptr + i;
  char *q = realloc (p, n);   // { dg-warning "\\\[-Wfree-nonheap-object" }
  sink (q);
}


void nowarn_realloc_parm_offset (char *p, int i)
{
  char *q = p + i;
  q = realloc (q, n);
  sink (q);
}

void nowarn_realloc_parm_neg_offset (char *p, int i)
{
  if (i >= 0)
    i = -1;

  char *q = p + i;
  q = realloc (q, n);
  sink (q);
}

void warn_realloc_parm_pos_offset (char *p, int i)
{
  if (i <= 0)
    i = 1;

  char *q = p + i;
  q = realloc (q, n);         // { dg-warning "\\\[-Wfree-nonheap-object" }
  sink (q);
}

void nowarn_realloc_deref_parm_pos_offset (void **p, int i)
{
  if (i <= 0)
    i = 1;

  // The offset is from p, not *p.
  void *q = *(p + i);
  q = realloc (q, n);
  sink (q);
}

void warn_realloc_deref_parm_pos_offset (void **p, int i)
{
  if (i <= 0)
    i = 1;

  // Unlike in the function above the offset is from *p.
  void *q = *p + i;
  q = realloc (q, n);         // { dg-warning "\\\[-Wfree-nonheap-object" }
  sink (q);
}
