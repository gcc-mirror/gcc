/* PR c++/71024 */
/* { dg-do compile } */
/* { dg-prune-output "declared but never defined" } */

void
fn0 (void) /* { dg-message "previous definition" } */
{
}
extern void __attribute__((optimize ("O2"))) fn0 (void); /* { dg-warning "optimization attribute" } */

extern __attribute__((noinline)) int fn1 (void); /* { dg-message "previous declaration" } */
extern inline int fn1 (void); /* { dg-warning "inline declaration of" } */

extern inline int fn2 (void); /* { dg-message "previous declaration" } */
extern __attribute__((noinline)) int fn2 (void); /* { dg-warning "attribute .noinline. follows inline declaration" } */

extern __attribute__((always_inline)) int fn3 (void); /* { dg-message "previous declaration" } */
extern __attribute__((noinline)) int fn3 (void); /* { dg-warning "attribute .noinline." } */

extern __attribute__((noinline)) int fn4 (void); /* { dg-message "previous declaration" } */
extern __attribute__((always_inline)) int fn4 (void); /* { dg-warning "attribute .always_inline." } */

extern __attribute__((hot)) int fn5 (void); /* { dg-message "previous declaration" } */
extern __attribute__((cold)) int fn5 (void); /* { dg-warning "attribute .cold." } */

extern __attribute__((cold)) int fn6 (void); /* { dg-message "previous declaration" } */
extern __attribute__((hot)) int fn6 (void); /* { dg-warning "attribute .hot." } */
