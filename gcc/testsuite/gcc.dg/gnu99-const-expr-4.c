/* Test for constant expressions: cases involving VLAs and typeof, at
   file scope.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic-errors" } */

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
int m;
int n;

static int (*a1)[] = &sa;
static int (*a2)[] = (__typeof__(int (*)[n]))sa;
static int (*a4)[] = (__typeof__((int (*)[n])sa))sa;
static int (*a5)[] = (__typeof__((int (*)[m++])sa))sa; /* { dg-error "constant" } */
static int (*a6)[] = (__typeof__((int (*)[100])(int (*)[m++])sa))sa;
static int (*a7)[] = (__typeof__((int (*)[n])sa + m++))sa; /* { dg-error "constant" } */
