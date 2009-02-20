// { dg-do assemble  }
// { dg-options "-Wno-abi" { target arm_eabi } }
// Bug: Checking whether A depends on template parms, we crash because
// __builtin_va_list lacks TYPE_LANG_SPECIFIC.


void f (__builtin_va_list arg)
{
  enum { a } A;
}
