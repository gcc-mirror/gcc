/* On VxWorks, in kernel mode, there is no support for .ctors/.dtors.
   Instead, initialization is handled by munch.  */

/* { dg-do compile { target vxworks_kernel } } */
/* { dg-skip-if "vxworks7 SR06x0 now uses .init_array" { *-*-vxworks7r* } } */
/* { dg-final { scan-assembler-not "\.ctors" } } */
/* { dg-final { scan-assembler-not "\.dtors" } } */

volatile int i;

void c1 () __attribute__((constructor));
void c1 () { ++i; }

void d1 () __attribute__((destructor));
void d1 () { --i; }

