/* PR middle-end/77721 - -Wformat-length not uses arg range for converted vars
   Test to verify that the correct range information is made available to the
   -Wformat-lenght check to prevent warnings.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wformat -Wformat-length" } */

int snprintf (char*, __SIZE_TYPE__, const char*, ...);

void fuchar (unsigned char j, char *p)
{
  if (j > 99)
    return;
  snprintf (p, 4, "%3hu", j);
}

void fschar (signed char j, char *p)
{
  const unsigned char k = (unsigned char) j;

  if (k > 99)
    return;

  snprintf (p, 3, "%3hhu", k);   /* { dg-bogus "" "unsigned char" { xfail *-*-* } } */
}

void fushrt (unsigned short j, char *p)
{
  if (j > 999)
    return;
  snprintf (p, 4, "%3hu", j);
}

void fshrt (short j, char *p)
{
  const unsigned short k = (unsigned short) j;

  if (k > 999)
    return;

  snprintf (p, 4, "%3hu", k);
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

  snprintf (p, 4, "%3u", k);   /* { dg-bogus "" "unsigned int" { xfail *-*-* } } */
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

  snprintf (p, 4, "%3lu", k);   /* { dg-bogus "" "unsigned long" { xfail *-*-* } } */
}

void fullong (unsigned long long j, char *p)
{
  if (j > 999)
    return;
  snprintf (p, 4, "%3llu", j);
}

void fllong (long j, char *p)
{
  const unsigned long long k = (unsigned long long) j;

  if (k > 999)
    return;

  snprintf (p, 4, "%3llu", k);   /* { dg-bogus "" "unsigned long long" { xfail *-*-* } } */
}
