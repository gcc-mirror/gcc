/* PR middle-end/77721 - -Wformat-overflow not uses arg range for converted vars
   Test to verify that the correct range information is made available to the
   -Wformat-lenght check to prevent warnings.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wformat -Wformat-overflow -fdump-tree-optimized" } */

void abort (void);
int snprintf (char*, __SIZE_TYPE__, const char*, ...);

void fuchar (unsigned char j, char *p)
{
  if (j > 99)
    return;

  if (3 != snprintf (p, 4, "%3hu", j))
    abort ();
}

void fschar (signed char j, char *p)
{
  const unsigned char k = (unsigned char) j;

  if (k > 99)
    return;

  if (3 != snprintf (p, 4, "%3hhu", k))
    abort ();
}

void fushrt (unsigned short j, char *p)
{
  if (j > 999)
    return;

  if (3 != snprintf (p, 4, "%3hu", j))
    abort ();
}

void fshrt (short j, char *p)
{
  const unsigned short k = (unsigned short) j;

  if (k > 999)
    return;

  if (3 != snprintf (p, 4, "%3hu", k))
    abort ();
}

void fuint (unsigned j, char *p)
{
  if (j > 999)
    return;

  snprintf (p, 4, "%3u", j);
}

void fint (int j, char *p)
{
  const unsigned k = (unsigned) j;

  if (k > 999)
    return;

  snprintf (p, 4, "%3u", k);
}

void fulong (unsigned long j, char *p)
{
  if (j > 999)
    return;

  snprintf (p, 4, "%3lu", j);
}

void flong (long j, char *p)
{
  const unsigned long k = (unsigned long) j;

  if (k > 999)
    return;

  snprintf (p, 4, "%3lu", k);
}

void fullong (unsigned long long j, char *p)
{
  if (j > 999)
    return;

  snprintf (p, 4, "%3llu", j);
}

void fllong (long long j, char *p)
{
  const unsigned long long k = (unsigned long long) j;

  if (k > 999)
    return;

  snprintf (p, 4, "%3llu", k);
}

/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */
