/* { dg-do compile } */

/* https://gcc.gnu.org/bugzilla/show_bug.cgi?id=106459 */

typedef unsigned int UDItype __attribute__((mode(DI)));
int foo(UDItype x) {
  x = x & (((UDItype)(((UDItype)(((UDItype)0x0F << 8) | 0x0F) << (2 * 8)) |
                      (((UDItype)0x0F << 8) | 0x0F))
            << (4 * 8)) |
           (((UDItype)(((UDItype)0x0F << 8) | 0x0F) << (2 * 8)) |
            (((UDItype)0x0F << 8) | 0x0F)));
  return x;
}
