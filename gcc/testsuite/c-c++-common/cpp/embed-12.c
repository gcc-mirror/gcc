/* { dg-do preprocess } */
/* { dg-options "" } */

#if __has_embed (__FILE__ limit (-1)) /* { dg-error "negative embed parameter operand" } */
#endif
#if __has_embed (__FILE__ limit (-42)) /* { dg-error "negative embed parameter operand" } */
#endif
#if __has_embed (__FILE__ limit (-9223372036854775807 - 1)) /* { dg-error "negative embed parameter operand" } */
#endif
#if __has_embed (__FILE__ limit (18446744073709551615ULL))
#endif
#if __has_embed (__FILE__ limit (18446744073709551615ULL + 42))
#endif
#embed __FILE__ limit (-1) /* { dg-error "negative embed parameter operand" } */
#embed __FILE__ limit (-42) /* { dg-error "negative embed parameter operand" } */
#embed __FILE__ limit (-9223372036854775807 - 1) /* { dg-error "negative embed parameter operand" } */
#embed __FILE__ limit (18446744073709551615ULL)
#embed __FILE__ limit (18446744073709551615ULL + 42)
