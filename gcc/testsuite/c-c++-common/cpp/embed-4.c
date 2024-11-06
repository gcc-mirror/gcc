/* { dg-do preprocess } */
/* { dg-options "" } */

#if 1 + __has_embed (__FILE__ , limit(1)) /* { dg-error "expected parameter name" } */
/* { dg-error "missing binary operator before token 'limit'" "" { target *-*-* } .-1 } */
#endif
#if 1 + __has_embed (__FILE__ limit(1) /* { dg-error "expected '\\\)'" } */
#endif
#if 1 + __has_embed (__FILE__ limit(1) limit(1)) /* { dg-error "duplicate embed parameter 'limit'" } */
#endif
#if 1 + __has_embed (__FILE__ limit(1) prefix() prefix()) /* { dg-error "duplicate embed parameter 'prefix'" } */
#endif
#if 1 + __has_embed (__FILE__ limit(1) suffix(1 2 3) __suffix__()) /* { dg-error "duplicate embed parameter 'suffix'" } */
#endif
#if 1 + __has_embed (__FILE__ if_empty ("this") if_empty("that") limit(1)) /* { dg-error "duplicate embed parameter 'if_empty'" } */
#endif
#if 1 + __has_embed (__FILE__ limit prefix() suffix()) /* { dg-error "expected '\\\('" } */
#endif
#if 1 + __has_embed (__FILE__ __prefix__ suffix() limit(1)) /* { dg-error "expected '\\\('" } */
#endif
#if 1 + __has_embed (__FILE__ prefix() suffix limit(1)) /* { dg-error "expected '\\\('" } */
#endif
#if 1 + __has_embed (__FILE__ if_empty limit(1)) /* { dg-error "expected '\\\('" } */
#endif
#if 1 + __has_embed (__FILE__ limit (1 / 0)) /* { dg-error "division by zero in #embed" } */
#endif
#if 1 + __has_embed (__FILE__ limit (+ + +)) /* { dg-error "operator '\\\+' has no right operand" } */
#endif
#if 1 + __has_embed (__FILE__ limit(1) prefix(()) /* { dg-error "expected '\\\)'" } */
#endif
#if 1 + __has_embed (__FILE__ limit(1) prefix(({[})])) /* { dg-error "unbalanced '\\\('" } */
/* { dg-error "unbalanced '\\\)'" "" { target *-*-* } .-1 } */
/* { dg-error "unbalanced '\\\{'" "" { target *-*-* } .-2 } */
/* { dg-error "unbalanced '\\\}'" "" { target *-*-* } .-3 } */
/* { dg-error "expected '\\\)'" "" { target *-*-* } .-4 } */
#endif
#if 1 + __has_embed (__FILE__ limit(1) prefix(])) /* { dg-error "unbalanced '\\\]'" } */
#endif
#if 1 + __has_embed (__FILE__ limit(1) prefix([)) /* { dg-error "unbalanced '\\\)'" } */
/* { dg-error "unbalanced '\\\['" "" { target *-*-* } .-1 } */
/* { dg-error "unbalanced '\\\('" "" { target *-*-* } .-2 } */
/* { dg-error "expected '\\\)'" "" { target *-*-* } .-3 } */
#endif
#if 1 + __has_embed (__FILE__ limit(1) __suffix__(()) /* { dg-error "expected '\\\)'" } */
#endif
#if 1 + __has_embed (__FILE__ limit(1) __suffix__(({[})])) /* { dg-error "unbalanced '\\\('" } */
/* { dg-error "unbalanced '\\\)'" "" { target *-*-* } .-1 } */
/* { dg-error "unbalanced '\\\{'" "" { target *-*-* } .-2 } */
/* { dg-error "unbalanced '\\\}'" "" { target *-*-* } .-3 } */
/* { dg-error "expected '\\\)'" "" { target *-*-* } .-4 } */
#endif
#if 1 + __has_embed (__FILE__ limit(1) suffix([)) /* { dg-error "unbalanced '\\\)'" } */
/* { dg-error "unbalanced '\\\['" "" { target *-*-* } .-1 } */
/* { dg-error "unbalanced '\\\('" "" { target *-*-* } .-2 } */
/* { dg-error "expected '\\\)'" "" { target *-*-* } .-3 } */
#endif
#if 1 + __has_embed (__FILE__ limit(1) if_empty(()) /* { dg-error "expected '\\\)'" } */
#endif
#if 1 + __has_embed (__FILE__ limit(1) __if_empty__(({[})])) /* { dg-error "unbalanced '\\\('" } */
/* { dg-error "unbalanced '\\\)'" "" { target *-*-* } .-1 } */
/* { dg-error "unbalanced '\\\{'" "" { target *-*-* } .-2 } */
/* { dg-error "unbalanced '\\\}'" "" { target *-*-* } .-3 } */
/* { dg-error "expected '\\\)'" "" { target *-*-* } .-4 } */
#endif
#if 1 + __has_embed (__FILE__ limit(1) __if_empty__([)) /* { dg-error "unbalanced '\\\)'" } */
/* { dg-error "unbalanced '\\\['" "" { target *-*-* } .-1 } */
/* { dg-error "unbalanced '\\\('" "" { target *-*-* } .-2 } */
/* { dg-error "expected '\\\)'" "" { target *-*-* } .-3 } */
#endif
#if 1 + __has_embed (__FILE__ limit(1) gnu::non_existent_parameter(()) /* { dg-error "expected '\\\)'" } */
#endif
#if 1 + __has_embed (__FILE__ limit(1) __gnu__::__non_existent_parameter__(({[})])) /* { dg-error "unbalanced '\\\('" } */
/* { dg-error "unbalanced '\\\)'" "" { target *-*-* } .-1 } */
/* { dg-error "unbalanced '\\\{'" "" { target *-*-* } .-2 } */
/* { dg-error "unbalanced '\\\}'" "" { target *-*-* } .-3 } */
/* { dg-error "expected '\\\)'" "" { target *-*-* } .-4 } */
#endif
#if 1 + __has_embed (__FILE__ limit(1) __gnu__::__non_existent_parameter__([)) /* { dg-error "unbalanced '\\\)'" } */
/* { dg-error "unbalanced '\\\['" "" { target *-*-* } .-1 } */
/* { dg-error "unbalanced '\\\('" "" { target *-*-* } .-2 } */
/* { dg-error "expected '\\\)'" "" { target *-*-* } .-3 } */
#endif
#define FOO 1
#if 1 + __has_embed (limit(1)) /* { dg-error "operator '__has_embed' requires a header-name" } */
/* { dg-error "missing binary operator before token '1'" "" { target *-*-* } .-1 } */
#endif
#if 1 + __has_embed (__FILE__ limit(0 + defined(FOO))) /* { dg-error "'defined' in '#embed' parameter" } */
#endif
int a = __has_embed (__FILE__); /* { dg-error "'__has_embed' used outside of preprocessing directive" } */
#if __has_embed /* { dg-error "missing '\\\(' before '__has_embed' operand" } */
/* { dg-error "operator '__has_embed' requires a header-name" "" { target *-*-* } .-1 } */
#endif
#if __has_embed( /* { dg-error "operator '__has_embed' requires a header-name" } */
#endif
#if __has_embed() /* { dg-error "operator '__has_embed' requires a header-name" } */
#endif
#if __has_embed(")
/* { dg-warning "missing terminating \\\" character" "" { target *-*-* } .-1 } */
/* { dg-error "operator '__has_embed' requires a header-name" "" { target *-*-* } .-2 } */
#endif
#if __has_embed(<)
/* { dg-error "missing terminating '>' character" "" { target *-*-* } .-1 } */
/* { dg-error "expected '\\\)'" "" { target *-*-* } .-2 } */
#endif
#if __has_embed(>) /* { dg-error "operator '__has_embed' requires a header-name" } */
#endif
#if __has_embed("") /* { dg-error "empty filename in '__has_embed'" } */
#endif
#if __has_embed(<>) /* { dg-error "empty filename in '__has_embed'" } */
#endif
#if __has_embed(embed-4.c) /* { dg-error "operator '__has_embed' requires a header-name" } */
/* { dg-error "missing binary operator before token '4.c'" "" { target *-*-* } .-1 } */
#endif
#if __has_embed(__FILE__ foo:) /* { dg-error "expected parameter name" } */
/* { dg-error "missing '\\\(' in expression" "" { target *-*-* } .-1 } */
#endif
#if __has_embed(__FILE__ bar::) /* { dg-error "expected parameter name" } */
#endif
#if __has_embed(__FILE__ ::) /* { dg-error "expected parameter name" } */
/* { dg-error "missing '\\\(' in expression" "" { target *-*-* } .-1 } */
#endif
#if __has_embed(__FILE__ foo:bar) /* { dg-error "expected parameter name" } */
/* { dg-error "missing binary operator before token 'bar'" "" { target *-*-* } .-1 } */
#endif
#if __has_embed(__FILE__ foo::bar::baz) /* { dg-error "expected parameter name" } */
/* { dg-error "missing binary operator before token 'baz'" "" { target *-*-* } .-1 } */
#endif
#if __has_embed(__FILE__ foo : : bar) /* { dg-error "expected parameter name" } */
/* { dg-error "':' without preceding '\\\?'" "" { target *-*-* } .-1 } */
#endif
#if __has_embed(__FILE__ 42) /* { dg-error "expected parameter name" } */
/* { dg-error "missing '\\\(' in expression" "" { target *-*-* } .-1 } */
#endif
#if __has_embed(__FILE__ 42::foo) /* { dg-error "expected parameter name" } */
/* { dg-error "token '::' is not valid in preprocessor expressions" "" { target *-*-* } .-1 } */
#endif
#if __has_embed(__FILE__ foo::42) /* { dg-error "expected parameter name" } */
/* { dg-error "missing '\\\(' in expression" "" { target *-*-* } .-1 } */
#endif
#if __has_embed(__FILE__ limit(1/0)) /* { dg-error "division by zero in #embed" } */
#endif
