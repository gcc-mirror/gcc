/* PR c/35750 */

void foo(int[]);
void foo(x) int x[](); {} /* { dg-error "array of functions" } */
