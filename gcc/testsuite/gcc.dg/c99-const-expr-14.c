/* Test for constant expressions: cases involving VLAs, at file scope.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

/* It appears address constants may contain casts to variably modified
   types.  Whether they should be permitted was discussed in
   <http://groups.google.com/group/comp.std.c/msg/923eee5ab690fd98>
   <LV7g2Vy3ARF$Ew9Q@romana.davros.org>; since static pointers to VLAs
   are definitely permitted within functions and may be initialized
   and such initialization involves implicit conversion to a variably
   modified type, allowing explicit casts seems appropriate.  Thus,
   GCC allows them as long as the "evaluated" size expressions do not
   contain the various operators not permitted to be evaluated in a
   constant expression, and as long as the result is genuinely
   constant (meaning that pointer arithmetic using the size of the VLA
   is generally not permitted).  */

static int sa[100];

volatile int nv;
int m;
int n;
int f (int, int);

static int (*a2)[] = (int (*)[n])sa;
static int (*a8)[] = (int (*)[(m=n)])sa; /* { dg-error "constant" } */
static int (*a9)[] = (int (*)[(m+=n)])sa; /* { dg-error "constant" } */
static int (*a10)[] = (int (*)[f(m,n)])sa; /* { dg-error "constant" } */
static int (*a11)[] = (int (*)[(m,n)])sa; /* { dg-error "constant" } */
static int (*a12)[] = (int (*)[sizeof(int[n])])sa;
static int (*a13)[] = (int (*)[sizeof(int[m++])])sa; /* { dg-error "constant" } */
static int (*a15)[] = (int (*)[sizeof(*(int (*)[n])sa)])sa;
static int (*a16)[] = (int (*)[sizeof(*(int (*)[m++])sa)])sa; /* { dg-error "constant" } */
static int (*a17)[] = (int (*)[nv])sa;
