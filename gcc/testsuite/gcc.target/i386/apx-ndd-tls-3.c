/* PR target/113711 */
/* { dg-do assemble { target { apxf && { { ! ia32 } && { ! *-*-darwin* } } } } } */
/* { dg-require-effective-target tls } */
/* { dg-options "-mapxf -mtune-ctrl=enable_ndd_mem -O2" } */

typedef signed __int128 S;
__thread S var;
int o;

S
qux (void)
{
  S z;
  o = __builtin_add_overflow (var, 0x200, &z);
  return z;
}
