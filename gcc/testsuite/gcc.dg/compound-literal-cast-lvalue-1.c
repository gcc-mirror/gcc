/* { dg-do compile } */
/* { dg-options "-std=c99" } */
/* PR c/84900; casts from compound literals
   were not considered a non-lvalue. */

int main() {
        int *p = &(int) (int) {0}; /* { dg-error "lvalue" } */
        return 0;
}
