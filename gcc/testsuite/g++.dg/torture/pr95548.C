/* { dg-do compile } */
enum a { b = (unsigned long)-1 } c;
#ifdef __SIZEOF_INT128__
enum c { d = (unsigned long)-1 } e;
#endif
main()
{
}
