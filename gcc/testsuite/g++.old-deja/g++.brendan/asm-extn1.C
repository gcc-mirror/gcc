// { dg-do assemble { target sparc*-*-* } }
// { dg-options "-S" }
// GROUPS passed asm-extension
// This used to crash because c_expand_asm_keyword didn't know what to
// do with this.  The parser rules were changed to accept an expr, instead
// of a stmt.

extern void traptable(void);

int main()
{
  asm("wr    %0,%%tbr" : : "r" (traptable));
}
