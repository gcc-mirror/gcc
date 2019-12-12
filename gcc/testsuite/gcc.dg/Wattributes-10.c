/* PR middle-end/86453 - error: type variant differs by TYPE_PACKED in
   free_lang_data since r255469
   { dg-do compile }
   { dg-options "-Wall -ftrack-macro-expansion=0" }
   { dg-additional-options "-fno-common" { target hppa*-*-hpux* } } */

#define A(expr) do { int a[1 - 2 * !(expr)]; (void)&a; } while (0)

struct S
{
  int* __attribute__ ((aligned (16))) paligned;
  int* __attribute__ ((packed)) ppacked;                  /* { dg-warning ".packed. attribute ignored for type .int \\\*." } */

  int* __attribute__ ((aligned (16), packed)) qaligned;   /* { dg-warning "ignoring attribute .packed. because it conflicts with attribute .aligned." } */
  int* __attribute__ ((packed, aligned (16))) qpacked;    /* { dg-warning ".packed. attribute ignored for type .int \\\*." } */
} s;    /* { dg-error "alignment of 's' is greater" "" { target pdp11*-*-* } } */


void test (void)
{
  /* Verify that attributes reported ignored really are ignored
     and not applied.  */

  A (__alignof__ (s.paligned) == 16);
  A (__alignof__ (s.ppacked) < 16);
  A (__alignof__ (s.qaligned) == 16);
  A (__alignof__ (s.qpacked) == __alignof__ (s.paligned));
}
