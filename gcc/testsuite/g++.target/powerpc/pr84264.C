/* { dg-do compile } */
/* { dg-options "-w -O1 -fstack-protector-strong" } */

void _setjmp ();
void a (unsigned long *);
void
b (void)
{
  for (;;)
    {
      _setjmp ();
      unsigned long args[9]{};
      a (args);
    }
}
