/* PR target/113711 */
/* { dg-do assemble { target { apxf && { ! ia32 } } } } */
/* { dg-options "-mapxf -O2" } */

unsigned __int128
foo (void)
{
  return *((unsigned __int128 __seg_fs *) 0x1000) + 0x2000;
}
