/* Verify that offsets in "anti-ranges" are handled correctly.
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0 -fno-ipa-icf" } */

typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef __SIZE_TYPE__    size_t;

void* memset (void*, int, size_t);

void sink (void*, ...);
#define T(x) sink (x)


void int_range_add_sub_ (int i, int j)
{
  if (i < 1) i = 1;
  if (j > -1) j = -1;

  char ca5[5];              // { dg-message "at offset \\\[1, 5]" "note" }
  char *p0 = ca5;           // offset
  char *p1 = p0 + i;        //  1-5
  char *p2 = p1 + i;        //  2-5
  char *p3 = p2 + j;        //  0-4
  char *p4 = p3 + j;        //  0-3
  char *p5 = p4 + j;        //  0-2
  char *p6 = p5 + j;        //  0-1
  char *p7 = p6 + i;        //  1-2

  memset (p7, 0, 5);        // { dg-warning "writing 5 bytes into a region of size 4" }

  sink (p0, p1, p2, p3, p4, p5, p6, p7);
}


void ruint_arint_add (unsigned i, int j)
{
  i |= 1;   // [1, UINT_MAX]
  j |= 1;   // [INT_MIN + 1, -1] U [1, INT_MAX]

  char a[5];                // { dg-message "at offset \\\[1, 5]" "note" }
  char *p0 = a;             // offset
  char *p1 = p0 + i;        //  1-5
  T (memset (p1, 0, 4));

  char *p2 = p1 + j;        //  0-5
  T (memset (p2, 0, 5));

  char *p3 = p2 + i;        //  1-5
  T (memset (p3, 0, 4));

  char *p4 = p3 + j;        //  0-5
  T (memset (p4, 0, 5));

  char *p5 = p4 + i;        //  1-5
  T (memset (p5, 0, 4));

  char *p6 = p5 + j;        //  0-5
  T (memset (p6, 0, 5));

  char *p7 = p6 + i;        //  1-5
  T (memset (p7, 0, 5));    // { dg-warning "writing 5 bytes into a region of size 4" "" }
}


void warn_ptrdiff_anti_range_add (ptrdiff_t i)
{
  i |= 1;

  char ca5[5];              // { dg-message "at offset \\\[1, 5]" "pr?????" }
  char *p0 = ca5;           // offset
  char *p1 = p0 + i;        //  1-5
  char *p2 = p1 + i;        //  2-5
  char *p3 = p2 + i;        //  3-5
  char *p4 = p3 + i;        //  4-5
  char *p5 = p4 + i;        //   5

  memset (p5, 0, 5);        // { dg-warning "writing 5 bytes into a region of size" "pr?????" }

  sink (p0, p1, p2, p3, p4, p5);
}

void warn_int_anti_range (int i)
{
  i |= 1;

  char ca5[5];              // { dg-message "at offset \\\[1, 5]" "pr?????" }
  char *p0 = ca5;           // offset
  char *p1 = p0 + i;        //  1-5
  char *p2 = p1 + i;        //  2-5
  char *p3 = p2 + i;        //  3-5
  char *p4 = p3 + i;        //  4-5
  char *p5 = p4 + i;        //   5

  memset (p5, 0, 5);        // { dg-warning "writing 5 bytes into a region of size" "pr?????" }

  sink (p0, p1, p2, p3, p4, p5);
}
