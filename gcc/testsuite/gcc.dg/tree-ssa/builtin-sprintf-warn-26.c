/* PR middle-end/100307 - spurious -Wplacement-new with negative pointer
   offset
   { dg-do compile }
   { dg-options "-O0 -Wall" } */

extern int sprintf (char*, const char*, ...);

char a[4];

void nowarn_1m1 ()
{
  char *p = a + 1;
  sprintf (p - 1, "%i", 123);   // { dg-bogus "-Wformat-overflow" }
}

void nowarn_4m3 ()
{
  char *p = a + 4;
  sprintf (p - 3, "%i", 12);    // { dg-bogus "-Wformat-overflow" }
}

void warn_2m1 ()
{
  char *p = a + 2;
  sprintf (p - 1, "%i", 123);   // { dg-warning "-Wformat-overflow" "pr100325" }
}

void warn_3m1 ()
{
  char *p = a + 3;
  sprintf (p - 1, "%i", 12);    // { dg-warning "-Wformat-overflow" "pr100325" }
}

void warn_4m1 ()
{
  char *p = a + 4;
  sprintf (p - 1, "%i", 1);     // { dg-warning "-Wformat-overflow" "pr100325" }
}
