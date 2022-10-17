/* PR preprocessor/103026 */
/* { dg-do compile } */

int main() {
    /* Say hello; newline⁧/*/ return 0 ;
/* { dg-warning "bidirectional" "" { target *-*-* } .-1 } */
    __builtin_printf("Hello world.\n");
    return 0;
}
