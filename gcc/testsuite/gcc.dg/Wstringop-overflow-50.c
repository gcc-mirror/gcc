/* Verify that writes at excessive offsets into objects of unknown size
   pointed to by function arguments are diagnosed.
   { dg-do compile }
   { dg-options "-O2" } */

#define DIFF_MAX __PTRDIFF_MAX__

typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef __SIZE_TYPE__    size_t;

void* memset (void*, int, size_t);

void sink (void*);

char* fcall (void);

void char_ptr_cst_off_cst_size (char *p)
                                        // { dg-message "at offset \[1-9\]\[0-9\]+ into destination object 'p'" "note" { target *-*-* } .-1 }
{
  size_t idx = DIFF_MAX - 3;

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


void char_ptr_var_difoff_cst_size (ptrdiff_t idx)
{
  char *p = fcall ();
  /* The offset is a range with a very large lower bound and an upper
     bound of DIFF_MAX.  There's not point in also mentioning the latter
     (it wouldn't make the note any more meaningful) so verify it only
     mentions the lower bound.
     { dg-message "at offset \\d+ into destination object of size \\\[0, \\d+] (allocated|returned) by 'fcall'" "note" { target *-*-* } .-5 } */

  if (idx < DIFF_MAX - 3)
    idx = DIFF_MAX - 3;

  memset (p + idx, 0, 3);
  sink (p);

  memset (p + idx, 0, 5);               // { dg-warning "writing 5 bytes into a region of size 3" }
}


void char_ptr_var_szoff_cst_size (size_t idx)
{
  extern char* gptr;
  // { dg-message "at offset \\d+ into destination object 'gptr'" "note" { target *-*-* } .-1 }

  char *p = gptr;

  if (idx < DIFF_MAX - 3)
    idx = DIFF_MAX - 3;

  memset (p + idx, 0, 3);
  sink (p);

  memset (p + idx, 0, 5);               // { dg-warning "writing 5 bytes into a region of size 3" "" { xfail *-*-* } }

  if (idx > DIFF_MAX)
    idx = DIFF_MAX;

  memset (p + idx, 0, 7);               // { dg-warning "writing 7 bytes into a region of size 3" }
}


void char_ptr_var_difoff_var_size (char *p, ptrdiff_t idx, size_t n)
                                        // { dg-message "at offset \\d+ into destination object 'p'" "note" { target *-*-* } .-1 }
{
  if (idx < DIFF_MAX - 3)
    idx = DIFF_MAX - 3;

  if (n < 3 || 7 < n)
    n = 3;

  memset (p + idx, 0, n);
  sink (p);

  ++n;
  memset (p + idx, 0, n);               // { dg-warning "writing between 4 and 8 bytes into a region of size 3" }
}


void char_ptr_var_szoff_var_size (char *p, size_t idx, size_t n)
                                        // { dg-message "at offset \\\[\[1-9\]\[0-9\]+, \[1-9\]\[0-9\]+] into destination object 'p'" "note" { xfail *-*-* } .-1 }
{
  if (idx < DIFF_MAX - 3)
    idx = DIFF_MAX - 3;

  if (n < 3 || 7 < n)
    n = 3;

  memset (p + idx, 0, n);
  sink (p);

  ++n;
  /* With an unsigned offset large values are interpreted as negative
     so the addition (p + idx) is effectively treated as subtraction,
     making an overflow indistinguishable from a valid (if unlikely)
     store.  */
  memset (p + idx, 0, n);               // { dg-warning "writing between 4 and 8 bytes into a region of size 3" "pr?????" { xfail *-*-* } }
}


void int_ptr_cst_off_cst_size (int *p)
                                        // { dg-message "at offset \[1-9\]\[0-9\]+ into destination object 'p'" "note" { target *-*-* } .-1 }
{
  size_t idx = DIFF_MAX / sizeof *p;

  memset (p + idx, 0, 3);
  sink (p);

  memset (p + idx, 0, 5);               // { dg-warning "writing 5 bytes into a region of size 3" }
}
