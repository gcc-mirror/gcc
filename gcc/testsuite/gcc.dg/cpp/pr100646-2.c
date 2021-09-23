/* PR preprocessor/100646 */
/* { dg-do compile } */
/* { dg-options "-fdirectives-only -save-temps -std=c17" } */
int main () { return 0; }
/* { dg-warning "backslash-newline at end of file" "" { target *-*-* } .+1 } */
// Not newline terminated\