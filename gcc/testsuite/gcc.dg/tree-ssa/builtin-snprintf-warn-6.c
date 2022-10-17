/* PR tree-optimization/78969 - bogus snprintf truncation warning due to
   missing range info
   { dg-do compile }
   { dg-options "-O2 -Wall -Wformat-truncation=2" } */

typedef __SIZE_TYPE__ size_t;

extern int snprintf (char*, size_t, const char*, ...);


void f (unsigned j, char *p)
{
  if (j > 999)
    j = 0;

  snprintf (p, 4, "%3u", j);
}

void g (unsigned j, char *p)
{
  if (j > 999)
    return;

  snprintf (p, 4, "%3u", j);            // { dg-bogus "-Wformat-truncation" }
}


void pr78969_c4 (char * p /* NNN\0" */)
{
  for (int idx = 0; idx < 1000; idx++) {
    // guaranteed to be in [0-999] range
    snprintf (p, 4, "%d", idx);         // { dg-bogus "-Wformat-truncation" }
  }
}


void sink (int, ...);

char d[4];

void pr78969_c12 (unsigned i)
{
  if (i >= 1000 && i < 10000)
    snprintf (d, 4, "%3d", i / 10);     // { dg-bogus "-Wformat-truncation" }
  else
    sink (i / 10 % 10);
}
