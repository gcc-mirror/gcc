/* Test ICE for statement expression returning no value as asm input (bug
   100501).  */
/* { dg-do compile } */
/* { dg-options "" } */

int x;
int g ();

void
f ()
{
  __asm__ ("" : : "m" (({}))); /* { dg-error "memory input 0 is not directly addressable" } */
  __asm__ ("" : : "m" (({ ; }))); /* { dg-error "memory input 0 is not directly addressable" } */
  __asm__ ("" : : "m" (({ (void) 0; }))); /* { dg-error "memory input 0 is not directly addressable" } */
  __asm__ ("" : : "m" (({ f (); }))); /* { dg-error "memory input 0 is not directly addressable" } */
  __asm__ ("" : : "m" (({ f (); f (); }))); /* { dg-error "memory input 0 is not directly addressable" } */
  __asm__ ("" : : "m" (({ x = g (); f (); }))); /* { dg-error "memory input 0 is not directly addressable" } */
  __asm__ ("" : : "m" (({ if (1) g (); }))); /* { dg-error "memory input 0 is not directly addressable" } */
  __asm__ ("" : : "m" (({ if (1) g (); else g (); }))); /* { dg-error "memory input 0 is not directly addressable" } */
  __asm__ ("" : : "m" (({ test : goto test; }))); /* { dg-error "memory input 0 is not directly addressable" } */
  __asm__ ("" : : "m" (({ return; }))); /* { dg-error "memory input 0 is not directly addressable" } */
  __asm__ ("" : : "m" (({ while (1); }))); /* { dg-error "memory input 0 is not directly addressable" } */
  __asm__ ("" : : "m" (({ do {} while (1); }))); /* { dg-error "memory input 0 is not directly addressable" } */
  __asm__ ("" : : "m" (({ for (;;); }))); /* { dg-error "memory input 0 is not directly addressable" } */
  __asm__ ("" : : "m" (({ switch (x); }))); /* { dg-error "memory input 0 is not directly addressable" } */
}
