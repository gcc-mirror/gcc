// Bug: Checking whether A depends on template parms, we crash because
// __builtin_va_list lacks TYPE_LANG_SPECIFIC.

// Build don't link:

void f (__builtin_va_list arg)
{
  enum { a } A;
}
