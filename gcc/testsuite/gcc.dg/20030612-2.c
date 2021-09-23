/* Derived from PR middle-end/168.  */

/* { dg-do compile } */
/* { dg-options "-W" } */

extern void foo ();

unsigned char uc;
unsigned short int usi;
unsigned int ui;


void bar()
{
  if (uc + usi >= ui)  /* { dg-bogus "between signed and unsigned" } */
    foo ();
  if (uc * usi >= ui)  /* { dg-bogus "between signed and unsigned" } */
    foo ();
}

