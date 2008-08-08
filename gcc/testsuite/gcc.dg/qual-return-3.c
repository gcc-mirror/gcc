/* Test for warnings for qualified function return types.  Bug 15052
   from Olatunji Ruwase (tjruwase at stanfordalumni.org): qualifiers
   should not be lost when merging declarations.  */

/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

int foo (); /* { dg-message "note: previous declaration" "different qualifiers" } */
const int foo () { return 0; } /* { dg-error "conflicting types" "different qualifiers" } */

void bar (void);
volatile void bar () { } /* { dg-warning "qualified|volatile" "different qualifiers" } */

volatile void baz (void);
void baz () { } /* { dg-warning "not compatible" "different qualifiers" } */
