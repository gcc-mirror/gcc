/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds" } */

extern void tramp ();

int is_trampoline (void* function) /* { dg-bogus "arrays of functions are not meaningful" } */
{
  void* tramp_address = tramp;
  if (!(((__UINTPTR_TYPE__)function & 3) == 2))
    return 0;
  return (((long *) ((char*)function - 2))[0]
	  == ((long *) ((char*)tramp_address-2))[0]); /* { dg-warning "outside array bounds" } */
}
/* { dg-warning "accessing data memory with program memory address.*" "" { target avr-*-* } .-2 } */
