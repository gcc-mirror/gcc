/* PR target/113711 */
/* { dg-do assemble { target { apxf && { ! ia32 } } } } */
/* { dg-require-effective-target tls } */
/* { dg-options "-mapxf -O2" } */

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
