/* { dg-do compile } */
/* { dg-options "-Wpadded" } */

/*
 * The struct is on single line, because C++ compiler emits the -Wpadded
 * warning at the first line of the struct definition, while the C compiler at
 * the last line. This way the test passes on both.
 *
 * Attribute aligned is needed for the test to pass on targets where
 * the default behaviour is to pack the struct and also on targets that align
 * 4 byte fields to 2 byte boundary.
 */
struct S { __UINT32_TYPE__ i; char c; } __attribute__((aligned(4))); /* { dg-warning "padding struct size to alignment boundary with 3 bytes" } */

