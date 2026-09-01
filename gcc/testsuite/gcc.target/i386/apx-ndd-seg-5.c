/* PR target/113711 */
/* { dg-do assemble { target { apxf && { { ! ia32 } && { ! *-*-darwin* } } } } } */
/* { dg-options "-mapxf -mtune-ctrl=enable_ndd_mem -O2" } */

#include <stdint.h>

extern int bar __attribute__((__visibility__ ("hidden")));

uintptr_t
foo (void)
{
  return (*(uintptr_t __seg_fs *) 0x1000) - (uintptr_t) &bar;
}
