/* PR tree-optimization/23960
   fold-const.c used to construct a comparison node with one pointer
   operand and one non-pointer operand.  */

void abort (void) __attribute__ ((noreturn));

void
foo (char *d, unsigned long int n)
{ 
  if (d + n > d)
    abort ();
} 
