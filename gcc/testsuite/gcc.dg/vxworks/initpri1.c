/* On VxWorks, in RTP mode, constructors and destructors go in named
   sections.  The section names must include the initialization
   priority, even for constructors and destructors with the default
   priority.  */

/* The selector below excludes VxWorks AE because AE does not support
   RTP mode.  */
/* { dg-do compile { target { *-*-vxworks* && { ! *-*-vxworksae* } } } } */
/* { dg-skip-if "vxworks7 SR06x0 now uses .init_array" { *-*-vxworks7r* } } */
/* { dg-options "-mrtp" } */
/* { dg-final { scan-assembler "ctors\.00000" } } */
/* { dg-final { scan-assembler "dtors\.00000" } } */

volatile int i;

void c1 () __attribute__((constructor));
void c1 () { ++i; }

void d1 () __attribute__((destructor));
void d1 () { --i; }
