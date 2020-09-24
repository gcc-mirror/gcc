/* PR middle-end/97175 - ICE on an excessive strncpy bound
   { dg-do compile }
   { dg-options "-O -Wall" } */

int n;

char *d;

void sink (void*);

/* Exercise calls with a destination of unknown size.  */

void f0 (const void *s)
{
  if (n > 0) return;
  __builtin_memcpy (d, s, n);       // eliminated
}

void f1 (const void *s)
{
  if (n > 0) return;
  __builtin_memmove (d, s, n);      // eliminated
}

void f2 (void)
{
  if (n > 0) return;
  __builtin_memset (d, 0, n);       // eliminated
}

void f3 (const char *s)
{
  if (n > 0) return;
  __builtin_strncpy (d, s, n);      // can be eliminated but isn't
}

void f4 (const char *s)
{
  if (n > 0) return;
  *d = 0;
  __builtin_strncat (d, s, n);      // can be eliminated but isn't
}


/* Exercise the same calls but with a declared destination object.  */

void g0 (const void *s)
{
  if (n > 0) return;
  char a[1];
  __builtin_memcpy (a, s, n);       // eliminated
  sink (a);
}

void g1 (const void *s)
{
  if (n > 0) return;
  char a[1];
  __builtin_memmove (a, s, n);      // eliminated
  sink (a);
}

void g2 (void)
{
  if (n > 0) return;
  char a[1];
  __builtin_memset (a, 0, n);       // eliminated
  sink (a);
}

void g3 (const char *s)
{
  if (n > 0) return;
  char a[1];
  __builtin_strncpy (a, s, n);      // can be eliminated but isn't
  sink (a);
}

void g4 (const char *s)
{
  if (n > 0) return;
  char a[1];
  *a = 0;
  __builtin_strncat (a, s, n);      // can be eliminated but isn't
  sink (a);
}


void h0 (const void *s)
{
  if (n > 0) return;
  d = __builtin_malloc (1);
  __builtin_memcpy (d, s, n);       // eliminated
}

void h1 (const void *s)
{
  if (n > 0) return;
  d = __builtin_malloc (1);
  __builtin_memmove (d, s, n);      // eliminated
}

void h2 (void)
{
  if (n > 0) return;
  d = __builtin_malloc (1);
  __builtin_memset (d, 0, n);       // eliminated
}

void h3 (const char *s)
{
  if (n > 0) return;
  d = __builtin_malloc (1);
  __builtin_strncpy (d, s, n);      // can be eliminated but isn't
}

void h4 (const char *s)
{
  if (n > 0) return;
  d = __builtin_malloc (1);
  *d = 0;
  __builtin_strncat (d, s, n);      // can be eliminated but isn't
}

/* The calls above that aren't eliminated trigger
     warning: specified size between INT_MAX and SIZE_MAX exceed maximum
              object size PTRDIFF_MAX
  { dg-prune-output "-Wstringop-overflow" }
  { dg-prune-output "-Wstringop-overread" } */
