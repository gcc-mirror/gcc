/* { dg-do compile } */
/* { dg-options "" } */

int foo (void)
{
    const char *key = "obscurelevelofabstraction";
    const char reverse_key[__builtin_strlen(key)] = {'\0'}; /* { dg-error "variable-sized object may not be initialized except with an empty initializer" } */
    return __builtin_strlen(reverse_key);
}
