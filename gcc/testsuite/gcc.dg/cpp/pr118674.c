/* PR preprocessor/118674 */
/* { dg-do preprocess } */
#define M(__VA_ARGS__, ...)
/* { dg-error "'__VA_ARGS__' can only appear in the expansion of a C99 variadic macro" "" { target *-*-* } .-1 } */
/* { dg-error "duplicate macro parameter '__VA_ARGS__'" "" { target *-*-* } .-2 } */
