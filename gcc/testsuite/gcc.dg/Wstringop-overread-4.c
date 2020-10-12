/* Verify -Wstringop-overread with a source pointer pointing either
   before the beginning or past the end of an object.
   { dg-do compile }
   { dg-options "-O -Wall" } */

typedef __SIZE_TYPE__ size_t;

size_t strlen (const char *);


extern char a[1];

volatile size_t n;

void len_si_1_max (int i)
{
  if (i < 1) i = 1;
  n = strlen (a + i);         // { dg-warning "reading 1 or more bytes from a region of size 0" }
  n = strlen (a + i + 1);     // { dg-warning "reading 1 or more bytes from a region of size 0" }
}

void len_ui_1_max (unsigned i)
{
  if (i < 1) i = 1;
  n = strlen (a + i);         // { dg-warning "reading 1 or more bytes from a region of size 0" }
  n = strlen (a + i + 1);     // { dg-warning "reading 1 or more bytes from a region of size 0" "" { xfail ilp32 } }
}

void len_sl_1_max (long i)
{
  if (i < 1) i = 1;
  n = strlen (a + i);         // { dg-warning "reading 1 or more bytes from a region of size 0" }
  n = strlen (a + i + 1);     // { dg-warning "reading 1 or more bytes from a region of size 0" }
}

void len_ul_1_max (unsigned long i)
{
  if (i < 1) i = 1;
  n = strlen (a + i);         // { dg-warning "reading 1 or more bytes from a region of size 0" }
  n = strlen (a + i + 1);     // { dg-warning "reading 1 or more bytes from a region of size 0" "" { xfail *-*-* } }
}


void len_si_min_m1 (int i)
{
  if (i > -1) i = -1;
  n = strlen (a + i - 1);     // { dg-warning "reading 1 or more bytes from a region of size 0" "" { xfail lp64 } }
  n = strlen (a + i);         // { dg-warning "reading 1 or more bytes from a region of size 0" "" { xfail *-*-* } }
  n = strlen (a + i + 2);
}

void len_sl_min_m1 (long i)
{
  if (i > -1) i = -1;
  n = strlen (a + i - 1);     // { dg-warning "reading 1 or more bytes from a region of size 0" }
  n = strlen (a + i);         // { dg-warning "reading 1 or more bytes from a region of size 0" "" { xfail *-*-* } }
  n = strlen (a + i + 2);
}
