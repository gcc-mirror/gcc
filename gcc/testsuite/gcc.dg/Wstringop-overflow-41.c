/* Verify that writes at excessive offsets into declared or allocated
   objects of unknown size are diagnosed.
   { dg-do compile }
   { dg-options "-O2" } */

#define DIFF_MAX __PTRDIFF_MAX__

typedef __SIZE_TYPE__ size_t;

void* malloc (size_t);
void* memcpy (void*, const void*, size_t);
void* memset (void*, int, size_t);

void sink (void*);


void char_array_cst_off_cst_size (void)
{
  extern char caxcc[];                  // { dg-message "at offset \\d+ into destination object 'caxcc'" }

  char *p = caxcc;
  size_t idx = DIFF_MAX - 3;

  memset (p + idx, 0, 3);
  sink (p);

  ++idx;
  memset (p + idx, 0, 3);               // { dg-warning "writing 3 bytes into a region of size 2" }
  sink (p);

  ++idx;
  memset (p + idx, 0, 3);               // { dg-warning "writing 3 bytes into a region of size 1" }
  sink (p);

  ++idx;
  memset (p + idx, 0, 3);               // { dg-warning "writing 3 bytes into a region of size 0" }
  sink (p);
}


void char_array_var_off_cst_size (size_t idx)
{
  /* The offset is a range with a very large lower bound and an upper
     bound of DIFF_MAX.  There's not point in also mentioning the latter
     (it wouldn't make the note any more meaningful) so verify it only
     mentions the lower bound.  */
  extern char caxvc[];                  // { dg-message "at offset \\d+ into destination object 'caxvc'" "note" }

  char *p = caxvc;

  if (idx < DIFF_MAX - 3)
    idx = DIFF_MAX - 3;

  memset (p + idx, 0, 3);
  sink (p);

  memset (p + idx, 0, 5);               // { dg-warning "writing 5 bytes into a region of size 3" }
  sink (p);
}


void char_array_var_off_var_size (size_t idx, size_t n)
{
  extern char caxvv[];                  // { dg-message "at offset \\d+ into destination object 'caxvv'" "note" }

  char *p = caxvv;

  if (idx < DIFF_MAX - 3)
    idx = DIFF_MAX - 3;

  if (n < 3 || 7 < n)
    n = 3;

  memset (p + idx, 0, n);
  sink (p);

  ++n;
  memset (p + idx, 0, n);               // { dg-warning "writing between 4 and 8 bytes into a region of size 3" }
  sink (p);
}


void alloc_array_var_off_cst_size (size_t n, size_t idx)
{
  char *p = malloc (n);                 // { dg-message "at offset \\d+ into destination object" "note" }

  if (idx < DIFF_MAX - 3)
    idx = DIFF_MAX - 3;

  memset (p + idx, 0, 3);
  sink (p);

  memset (p + idx, 0, 5);               // { dg-warning "writing 5 bytes into a region of size 3" }
  sink (p);
}


void int_array_cst_off_cst_size (void)
{
  extern int iaxc[];                    // { dg-message "at offset \[1-9\]\[0-9\]+ into destination object 'iaxc'" }

  int *p = iaxc;
  size_t idx = DIFF_MAX / sizeof *iaxc;

  memset (p + idx, 0, 3);
  sink (p);

  memset (p + idx, 0, 5);               // { dg-warning "writing 5 bytes into a region of size 3" }
  sink (p);
}


void* nowarn_anti_range_1 (char *p, char *q)
{
  size_t n = q - p;
  if (!n) return 0;

  char *d = __builtin_malloc (n + 1);
  memcpy (d, p, n + 1);                 // { dg-bogus "-Wstringop-overflow" }
  return d;
}
