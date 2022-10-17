/* PR target/99708 */
/* { dg-do compile } */

#ifdef __SIZEOF_IBM128__
__ibm128 f = 1.0;
#endif
#ifdef __SIZEOF_IEEE128__
__ieee128 g = 1.0;
#endif
long double h = 1.0;

void
foo (void)
{
#ifdef __SIZEOF_IBM128__
  f += 2.0;
#endif
#ifdef __SIZEOF_IEEE128__
  g += 2.0;
#endif
  h += 2.0;
}
