/* Test for string translation.  */
/* { dg-do compile }
   { dg-require-iconv "IBM1047" } 
   { dg-final { scan-assembler "foo" } } */
int main()
{
  unsigned long int *ptr;
  ptr = ((unsigned long int *)
         ( { void *stack_ptr;
           __asm__ __volatile__ ( "foo %0" : "=r" (stack_ptr) );
           (stack_ptr); } ) );
  return 0;
}
