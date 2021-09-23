/* PR middle-end/100325 - missing warning with -O0 on sprintf overflow with
   pointer plus offset
   { dg-do compile }
   { dg-options "-O0 -Wall" } */

#define S(n) (&"0123456789"[10 - n])

extern int sprintf (char*, const char*, ...);

char d[10];

void nowarn_d10_s9 ()
{
  sprintf (d, "%s", S (9));     // { dg-bogus "-Wformat-overflow" }
}

void warn_d10_s10 ()
{
  sprintf (d, "%s", S (10));    // { dg-warning "-Wformat-overflow" }
}
