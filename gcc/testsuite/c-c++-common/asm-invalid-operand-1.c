/* { dg-do compile } */
/* { dg-options "" } */
/* PR middle-end/123709 */

int test1(int a) {
    asm volatile("%i\n" /* { dg-error "operand number out of range" } */
                 "505x"
                 :
                 : "r"(a));
    return a;
}
