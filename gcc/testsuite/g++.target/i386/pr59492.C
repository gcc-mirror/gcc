// { dg-do assemble { target { fpic } } }
// { dg-options "-mx32 -fPIC" }
// { dg-require-ifunc "" }
// { dg-require-effective-target maybe_x32 }
// { dg-require-effective-target rdrand }

void
__throw_runtime_error(const char*) __attribute__((__noreturn__));
unsigned int
__attribute__ ((target("rdrnd")))
__x86_rdrand(void)
{
  unsigned int retries = 100;
  unsigned int val;
  while (__builtin_ia32_rdrand32_step(&val) == 0)
    if (--retries == 0)
      __throw_runtime_error(("random_device::__x86_rdrand(void)"));
  return val;
}
