/* { dg-do preprocess } */
/* { dg-options -pedantic-errors } */

#if 'a' != 'a' || '\001' != 1 || '\x12' != 0x12
#error a,1,0x12 /* { dg-bogus "#error" "basic charconst recognition" } */
#endif

#if 'a' != L'a' || L'\xfeed' != 0xfeed
#error L'a',0xfeed /* { dg-bogus "#error" "wide charconst recognition" } */
#endif

#if 'abcd' /* { dg-warning "multi-character character constant" "multi-character charconst" } */
#endif

#if 'abcdefghi' /* { dg-error "character constant (is )?too long" "charconst too long" } */
#endif

#if '' /* { dg-error "empty character constant" "empty charconst" } */
#endif
