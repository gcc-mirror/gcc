/* PR preprocessor/103026 */
/* { dg-do compile } */

int main() {
    const char* access_level = "user";
    if (__builtin_strcmp(access_level, "user‮ ⁦// Check if admin⁩ ⁦")) {
/* { dg-warning "bidirectional" "" { target *-*-* } .-1 } */
        __builtin_printf("You are an admin.\n");
    }
    return 0;
}
