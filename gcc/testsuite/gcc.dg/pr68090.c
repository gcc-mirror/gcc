/* PR c/68090 */
/* { dg-do compile } */
/* { dg-options "" } */

void
fn (int i)
{
  (int[(0, 1)]) { 0 }; /* { dg-error "compound literal has variable size" } */
  (int[i]) { 0 }; /* { dg-error "compound literal has variable size" } */
  (int[(0, i)]) { 0 }; /* { dg-error "compound literal has variable size" } */
  (int [][i]){ 0 }; /* { dg-error "compound literal has variable size" } */
  (int [][(1, 2)]){ 0 }; /* { dg-error "compound literal has variable size" } */
}
