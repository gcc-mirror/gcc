/* PR middle-end/100307 - spurious -Wplacement-new with negative pointer
   offset
   { dg-do compile }
   { dg-options "-O0 -Wall" } */

void* operator new (__SIZE_TYPE__, void *p) { return p; }
void* operator new[] (__SIZE_TYPE__, void *p) { return p; }

static char a[2];

void* nowarn_scalar ()
{
  char* p = a + 1;
  char *q = new (p - 1) char ();    // { dg-bogus "-Wplacement-new" }
  return q;
}


void* nowarn_array ()
{
  char* p = a + 1;
  char *q = new (p - 1) char[2];    // { dg-bogus "-Wplacement-new" }
  return q;
}

void* warn_scalar ()
{
  char* p = a + 1;
  char *q = new (p - 2) char ();    // { dg-warning "-Wplacement-new" "pr100325" { xfail *-*-* } }
  return q;
}


void* warn_array ()
{
  char* p = a + 1;
  char *q = new (p - 1) char[2];    // { dg-warning "-Wplacement-new" "pr100325" { xfail *-*-* } }
  return q;
}
