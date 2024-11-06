/* { dg-do preprocess } */
/* { dg-options "" } */

#embed __FILE__ gnu::offset(1) gnu::offset(1) /* { dg-error "duplicate embed parameter 'gnu::offset'" } */
#embed __FILE__ gnu::offset prefix() suffix() /* { dg-error "expected '\\\('" } */
#embed __FILE__ gnu::offset (1 / 0) /* { dg-error "division by zero in #embed" } */
#embed __FILE__ __gnu__::__offset__ (+ + +) /* { dg-error "operator '\\\+' has no right operand" } */
#define FOO 1
#embed __FILE__ gnu::offset(0 + defined(FOO)) /* { dg-error "'defined' in '#embed' parameter" } */
#embed __FILE__ gnu::offset (-1) /* { dg-error "negative embed parameter operand" } */
#embed __FILE__ gnu::offset (-42) /* { dg-error "negative embed parameter operand" } */
#embed __FILE__ gnu::offset (-9223372036854775807 - 1) /* { dg-error "negative embed parameter operand" } */
#embed __FILE__ gnu::offset (18446744073709551615ULL) /* { dg-error "too large 'gnu::offset' argument" } */
#if 1 + __has_embed (__FILE__ gnu::offset(1) __gnu__::__offset__(1)) /* { dg-error "duplicate embed parameter 'gnu::offset'" } */
#endif
#if 1 + __has_embed (__FILE__ __gnu__::__offset__ prefix() suffix()) /* { dg-error "expected '\\\('" } */
#endif
#if 1 + __has_embed (__FILE__ gnu::offset(1/0)) /* { dg-error "division by zero in #embed" } */
#endif
#if 1 + __has_embed (__FILE__ gnu::offset(+ + +)) /* { dg-error "operator '\\\+' has no right operand" } */
#endif
#if 1 + __has_embed (__FILE__ gnu::offset(0 + defined(FOO))) /* { dg-error "'defined' in '#embed' parameter" } */
#endif
#if 1 + __has_embed (__FILE__ gnu::offset (-1)) /* { dg-error "negative embed parameter operand" } */
#endif
#if 1 + __has_embed (__FILE__ gnu::offset (-42)) /* { dg-error "negative embed parameter operand" } */
#endif
#if 1 + __has_embed (__FILE__ gnu::offset (-9223372036854775807 - 1)) /* { dg-error "negative embed parameter operand" } */
#endif
#if 1 + __has_embed (__FILE__ gnu::offset (18446744073709551615ULL)) /* { dg-error "too large 'gnu::offset' argument" } */
#endif
