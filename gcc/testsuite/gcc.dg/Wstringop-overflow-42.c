/* Verify -Wstringop-overflow a with destination pointer pointing either
   before the beginning or past the end of an object.
  { dg-do compile }
  { dg-options "-O -Wall -Wno-array-bounds -Wno-restrict" } */

typedef __SIZE_TYPE__ size_t;

char* strcpy (char *, const char *);


extern char a[1];

volatile char *d;

void cpy_si_1_max (int i, const char *s)
{
  if (i < 1) i = 1;
  d = strcpy (a + i, s);      // { dg-warning "writing 1 or more bytes into a region of size 0" }
  d = strcpy (a + i + 1, s);  // { dg-warning "writing 1 or more bytes into a region of size 0" }
}

void cpy_ui_1_max (unsigned i, const char *s)
{
  if (i < 1) i = 1;
  d = strcpy (a + i, s);      // { dg-warning "writing 1 or more bytes into a region of size 0" }
  d = strcpy (a + i + 1, s);  // { dg-warning "writing 1 or more bytes into a region of size 0" "" { xfail { ! lp64 } } }
}

void cpy_sl_1_max (long i, const char *s)
{
  if (i < 1) i = 1;
  d = strcpy (a + i, s);      // { dg-warning "writing 1 or more bytes into a region of size 0" "" { target { ! ptr_eq_short } } }
  d = strcpy (a + i + 1, s);  // { dg-warning "writing 1 or more bytes into a region of size 0" "" { target { ! ptr_eq_short } } }
}

void cpy_ul_1_max (unsigned long i, const char *s)
{
  if (i < 1) i = 1;

  d = strcpy (a + i, s);      // { dg-warning "writing 1 or more bytes into a region of size 0" "" { target { ! ptr_eq_short } } }

  /* Because of integer wraparound the offset's range is [1, 0] so
     the overflow isn't diagnosed (yet).  */
  d = strcpy (a + i + 1, s);  // { dg-warning "writing 1 or more bytes into a region of size 0" "" { xfail *-*-* } }
}


void cpy_si_min_m1 (int i, const char *s)
{
  if (i > -1) i = -1;
  d = strcpy (a + i - 1, s);  // { dg-warning "writing 1 or more bytes into a region of size 0" }
  d = strcpy (a + i, s);      // { dg-warning "writing 1 or more bytes into a region of size 0" }
  d = strcpy (a + i + 2, s);
}

void cpy_sl_min_m1 (long i, const char *s)
{
  if (i > -1) i = -1;
  d = strcpy (a + i - 1, s);  // { dg-warning "writing 1 or more bytes into a region of size 0" "" { target { ! ptr_eq_short } } }
  d = strcpy (a + i, s);      // { dg-warning "writing 1 or more bytes into a region of size 0" "" { target { ! ptr_eq_short } } }
  d = strcpy (a + i + 2, s);
}
