/* { dg-do compile } */
/* { dg-options "" } */

/* ISO C99 flexible array members don't have a size.  GCC's zero-length
   array extension does.  */

struct f { int w; int x[0]; } f;
struct g { int w; int x[]; } g;

char test_gcc[sizeof (f.x) ? -1 : 1];
char test_iso[sizeof (g.x) ? -1 : 1]; /* { dg-error "incomplete type" "iso" } */
