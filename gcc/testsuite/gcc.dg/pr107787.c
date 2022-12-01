/* PR driver/107787 */
/* { dg-do compile } */
/* { dg-options "-O2 -Werror=array-bounds=1" } */
/* { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 } */

int a[10];     /* { dg-note "while referencing" } */

int* f(void) {

    a[-1] = 0; /* { dg-error "is below array bounds" } */

    return a;
}
