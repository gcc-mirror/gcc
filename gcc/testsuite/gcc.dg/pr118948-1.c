/* { dg-do compile } */
/* { dg-options "" } */

/* PR c/118948 */

/* Used to ICE in tree_expr_nonnegative_p after an error. */

void f(void) {
    int i; /* { dg-note "previous" } */
    for (i = 0; i < 2; i++) ;
    float i; /* { dg-error "conflicting types for" } */
}
