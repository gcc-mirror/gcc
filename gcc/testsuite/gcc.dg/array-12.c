/* { dg-do compile } */
/* { dg-options "" } */

/* ISO C99 flexible array members don't have a size.  GCC's zero-length
   array extension does.  */

typedef int T0[0];
typedef int T[];
struct f { int w; T0 x; } f;
struct g { int w; T x; } g;

char test_gcc[sizeof (f.x) ? -1 : 1];
char test_iso[sizeof (g.x) ? -1 : 1]; /* { dg-error "incomplete type" "iso" } */
