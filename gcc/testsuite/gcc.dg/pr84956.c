/* { dg-options "-O2 -ftree-tail-merge" } */

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
  a && ({ setjmp (); });
  a && ({ setjmp (); });
}

