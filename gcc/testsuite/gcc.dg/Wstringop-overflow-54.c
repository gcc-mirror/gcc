/* Verify that writes at excessive offsets into flexible array members
   of extern or allocated objects of unknow size are diagnosed.
   { dg-do compile }
   { dg-options "-O2" } */

#define DIFF_MAX __PTRDIFF_MAX__

typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef __SIZE_TYPE__    size_t;

void* memset (void*, int, size_t);

void sink (void*);

void char_flexarray_cst_off_cst_size (void)
{
  extern struct { char n, a[]; }
    caxcc;                              // { dg-message "at offset \[1-9\]\[0-9\]+ into destination object 'caxcc'" "note" }

  char *p = caxcc.a;
  size_t idx = DIFF_MAX - 4;

  memset (p + idx, 0, 3);
  sink (p);

  ++idx;
  memset (p + idx, 0, 3);               // { dg-warning "writing 3 bytes into a region of size 2" }
  sink (p);

  ++idx;
  memset (p + idx, 0, 3);               // { dg-warning "writing 3 bytes into a region of size 1" }

  ++idx;
  memset (p + idx, 0, 3);               // { dg-warning "writing 3 bytes into a region of size 0" }
}


void char_flexarray_var_off_cst_size (ptrdiff_t idx)
{
  extern struct { char n, a[]; }
    caxvc;                              // { dg-message "destination object 'caxvc'" "note" }

  char *p = caxvc.a;

  if (idx < DIFF_MAX - 4)
    idx = DIFF_MAX - 4;

  memset (p + idx, 0, 3);
  sink (p);

  memset (p + idx, 0, 5);               // { dg-warning "writing 5 bytes into a region of size 3" }
}


void char_flexarray_var_off_var_size (size_t n, ptrdiff_t idx)
{
  extern struct { char n, a[]; }
    caxvv;                              // { dg-message "destination object 'caxvv'" "note" }

  char *p = caxvv.a;

  if (idx < DIFF_MAX - 4)
    idx = DIFF_MAX - 4;

  if (n < 3 || 7 < n)
    n = 3;

  memset (p + idx, 0, n);
  sink (p);

  ++n;
  memset (p + idx, 0, n);               // { dg-warning "writing between 4 and 8 bytes into a region of size 3" }
}


void alloc_array_var_off_cst_size (size_t n, ptrdiff_t idx)
{
  struct { char n, a[]; }
    *p = __builtin_malloc (n);          // { dg-message "at offset \\d+ into destination object" "note" }

  if (idx < DIFF_MAX - 4)
    idx = DIFF_MAX - 4;

  memset (p->a + idx, 0, 3);
  sink (p);

  memset (p->a + idx, 0, 5);            // { dg-warning "writing 5 bytes into a region of size 3" }
}


void int_array_cst_off_cst_size (void)
{
  extern struct { int n, a[]; }
    iaxc;                               // { dg-message "at offset \[1-9\]\[0-9\]+ into destination object 'iaxc'" "note" }

  int *p = iaxc.a;
  size_t idx = DIFF_MAX / sizeof *p - 1;

  memset (p + idx, 0, 3);
  sink (p);

  memset (p + idx, 0, 5);               // { dg-warning "writing 5 bytes into a region of size 3" }
}
