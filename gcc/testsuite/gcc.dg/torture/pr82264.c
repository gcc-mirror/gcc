/* { dg-do compile } */

char a;
int c;
unsigned b ();
unsigned
setjmp ()
{
}
static void
d ()
{
  if (b ())
    c = 3;
}
void
e ()
{
  d ();
  a && ({ setjmp (); });
}
