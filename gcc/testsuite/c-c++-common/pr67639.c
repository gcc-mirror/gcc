/* { dg-do compile } */
void
foo (int p)
{
  int t; 
  register long x asm ("rhubarb") = p; /* { dg-error "register name" } */
  __asm ("" : "=r" (t), "=r" (t), "=r" (t), "=r" (x) : "0" (x));
}
