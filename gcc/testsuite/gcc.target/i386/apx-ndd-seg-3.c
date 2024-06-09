/* PR target/113711 */
/* { dg-do assemble { target { apxf && { ! ia32 } } } } */
/* { dg-options "-mapxf -O2" } */

typedef signed __int128 S;
int o;

S
qux (void)
{
  S z;
  o = __builtin_add_overflow (*(S __seg_fs *) 0x1000, 0x200, &z);
  return z;
}
