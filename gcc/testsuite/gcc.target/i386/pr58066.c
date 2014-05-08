/* { dg-do compile } */
/* { dg-options "-fPIC -O2" } */

/* Check whether the stack frame starting addresses of tls expanded calls
   in foo and goo are 16bytes aligned.  */
static __thread char ccc1;
void* foo()
{
 return &ccc1;
}

__thread char ccc2;
void* goo()
{
 return &ccc2;
}

/* { dg-final { scan-assembler-times ".cfi_def_cfa_offset 16" 2 } } */
