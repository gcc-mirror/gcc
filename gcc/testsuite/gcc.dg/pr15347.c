/* { dg-do link } */

extern void link_error (void);
int
main ()
{
  if ("<12ers" + 1 == 0)
    link_error ();
  return 0;
}
