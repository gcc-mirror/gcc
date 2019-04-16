/* PR tree-optimization/89644 - False-positive -Warray-bounds diagnostic
   on strncpy
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" }  */

#define NONSTR __attribute__ ((nonstring))

typedef __SIZE_TYPE__ size_t;

size_t strlen (const char*);
extern char* stpncpy (char*, const char*, size_t);
extern char* strncpy (char*, const char*, size_t);

void sink (char*, ...);

char f0 (char *s)
{
  char a[6] NONSTR = { 1, 2, 3, 4, 5, 6 };
  if (*s)
    strncpy (a, s, sizeof a);       /* { dg-bogus "\\\[-Warray-bounds" } */
  return a[0];
}

void f1 (char *s)
{
  char a[6] NONSTR = { 1, 2, 3, 4, 5, 6 };
  if (*s)
    strncpy (a, s, sizeof a);       /* { dg-bogus "\\\[-Warray-bounds" } */
  sink (a);
}

char f2 (void)
{
  char a[6] NONSTR = { 1, 2, 3, 4, 5, 6 };
  char b[6] NONSTR = { 6, 5, 4, 3, 2, 1 };
  strncpy (a, b + 1, 5);            /* { dg-bogus "\\\[-Warray-bounds" } */
  return a[0];
}

void f3 (void)
{
  char a[6] NONSTR = { 1, 2, 3, 4, 5, 6 };
  char b[6] NONSTR = { 6, 5, 4, 3, 2, 1 };
  strncpy (a, b + 2, 4);            /* { dg-bogus "\\\[-Warray-bounds" } */
  sink (a);
}

void f4 (NONSTR char *d)
{
  char b[6] NONSTR = { 6, 5, 4, 3, 2, 1 };
  strncpy (d, b + 3, 3);            /* { dg-bogus "\\\[-Warray-bounds" } */
  sink (d);
}


char g0 (char *s)
{
  char a[6] NONSTR = { 1, 2, 3, 4, 5, 6 };
  if (*s)
    stpncpy (a, s, sizeof a);       /* { dg-bogus "\\\[-Warray-bounds" } */
  return a[0];
}

void g1 (char *s)
{
  char a[6] NONSTR = { 1, 2, 3, 4, 5, 6 };
  char *p = 0;
  if (*s)
    p = stpncpy (a, s, sizeof a);   /* { dg-bogus "\\\[-Warray-bounds" } */
  sink (a, p);
}

char g2 (void)
{
  char a[6] NONSTR = { 1, 2, 3, 4, 5, 6 };
  char b[6] NONSTR = { 6, 5, 4, 3, 2, 1 };
  stpncpy (a, b + 1, 5);            /* { dg-bogus "\\\[-Warray-bounds" } */
  return a[0];
}

void g3 (void)
{
  char a[6] NONSTR = { 1, 2, 3, 4, 5, 6 };
  char b[6] NONSTR = { 6, 5, 4, 3, 2, 1 };
  char *p = stpncpy (a, b + 2, 4);  /* { dg-bogus "\\\[-Warray-bounds" } */
  sink (a, p);
}

void g4 (NONSTR char *d)
{
  char b[6] NONSTR = { 6, 5, 4, 3, 2, 1 };
  char *p = stpncpy (d, b + 3, 3);  /* { dg-bogus "\\\[-Warray-bounds" } */
  sink (d, p);
}
