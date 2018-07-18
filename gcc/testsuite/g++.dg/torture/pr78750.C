// PR middle-end/78750
// { dg-do compile }

extern "C" char *strcpy (char *, const char *);

void
fn (char *p, char &as)
{
  strcpy (p, &as);
}
