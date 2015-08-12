/* { dg-do compile } */

extern int snprintf (char *, unsigned long, const char *, ...);
const char a[] = "";
int b;
void
get_bar ()
{
  snprintf (0, 0, "%s", &a[b]);
}
