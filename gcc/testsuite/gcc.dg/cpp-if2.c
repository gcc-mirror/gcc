/* { dg-do preprocess } */
/* { dg-options -pedantic-errors } */

#if 'a' == 'a' && '\001' == 1 && '\x12' == 0x12
#error yes /* { dg-error "#error yes" "basic charconst recognition" } */
#endif

#if 'a' == L'a' && L'\xfeed' == 0xfeed
#error yes /* { dg-error "#error yes" "wide charconst recognition" } */
#endif

#if 'abcd' /* { dg-warning "multi-character character constant" "multi-character charconst" } */
#endif

#if 'abcdefghi' /* { dg-error "character constant (is )?too long" "charconst too long" } */
#endif

#if '' /* { dg-error "empty character constant" "empty charconst" } */
#endif
