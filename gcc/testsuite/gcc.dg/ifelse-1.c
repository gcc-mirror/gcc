/*
{ dg-do compile }
{ dg-options "-Wno-old-style-definition -W -Wall" }
*/


extern int bar ();
extern int com ();
extern int baz ();
void
foo (a,b)
     int a, b;
{
  if (a)
    if (b)
      bar ();
    else
      com ();	/* { dg-bogus ".*warning.*" "bogus warning" } */
  else
    baz ();
}
