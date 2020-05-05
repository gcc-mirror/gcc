/* { dg-do compile } */

void (*baz_call)();
void baz ()
{
  baz_call ();
}
