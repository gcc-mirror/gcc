/* PR c/68090 */
/* { dg-do compile } */
/* { dg-options "--pedantic-error" } */

void
fn (int i)
{
  (int[(0, 1)]) { 0 }; /* { dg-error "compound literal has variable size" } */
  			/* { dg-error "variable-size" "" { target *-*-* } .-1 } */
  (int[i]) { 0 }; /* { dg-error "compound literal has variable size" } */
  			/* { dg-error "variable-size" "" { target *-*-* } .-1 } */
  (int[(0, i)]) { 0 }; /* { dg-error "compound literal has variable size" } */
  			/* { dg-error "variable-size" "" { target *-*-* } .-1 } */
  (int [][i]){ 0 }; /* { dg-error "compound literal has variable size" } */
  			/* { dg-error "variable-size" "" { target *-*-* } .-1 } */
  (int [][(1, 2)]){ 0 }; /* { dg-error "compound literal has variable size" } */
  			/* { dg-error "variable-size" "" { target *-*-* } .-1 } */
}
