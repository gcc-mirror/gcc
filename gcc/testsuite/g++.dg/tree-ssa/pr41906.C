/* { dg-do compile } */
/* { dg-options "-fpermissive -w" } */
/* We aren't interested in the warning, but in the ICE.  */
void foo();
extern void abort (void);

void bar()
{
  try { foo(); }
  catch (...) {}
  catch (int) {abort ();}
}
