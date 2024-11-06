/* { dg-do preprocess } */
/* { dg-options "" } */

#embed __FILE__ , limit(1) /* { dg-error "expected parameter name" } */
#embed __FILE__ limit(1)) /* { dg-error "expected parameter name" } */
#embed __FILE__ limit(1) limit(1) /* { dg-error "duplicate embed parameter 'limit'" } */
#embed __FILE__ limit(1) prefix() prefix() /* { dg-error "duplicate embed parameter 'prefix'" } */
#embed __FILE__ limit(1) suffix(1 2 3) __suffix__() /* { dg-error "duplicate embed parameter 'suffix'" } */
#embed __FILE__ if_empty ("this") if_empty("that") limit(1) /* { dg-error "duplicate embed parameter 'if_empty'" } */
#embed __FILE__ non_existent_parameter (42) limit(1) /* { dg-error "unknown embed parameter 'non_existent_parameter'" } */
#embed __FILE__ limit(1) gnu::non_existent_parameter /* { dg-error "unknown embed parameter 'gnu::non_existent_parameter'" } */
#embed __FILE__ limit(1) __gnu__::__non_existent_parameter /* { dg-error "unknown embed parameter 'gnu::__non_existent_parameter'" } */
#embed __FILE__ limit(1) gnu__::__non_existent_parameter__ /* { dg-error "unknown embed parameter 'gnu__::non_existent_parameter'" } */
#embed __FILE__ limit prefix() suffix() /* { dg-error "expected '\\\('" } */
#embed __FILE__ __prefix__ suffix() limit(1) /* { dg-error "expected '\\\('" } */
#embed __FILE__ prefix() suffix limit(1) /* { dg-error "expected '\\\('" } */
#embed __FILE__ if_empty limit(1) /* { dg-error "expected '\\\('" } */
#embed __FILE__ limit (1 / 0) /* { dg-error "division by zero in #embed" } */
#embed __FILE__ limit (+ + +) /* { dg-error "operator '\\\+' has no right operand" } */
#embed __FILE__ limit(1) prefix(() /* { dg-error "unbalanced '\\\('" } */
#embed __FILE__ limit(1) prefix(({[})]) /* { dg-error "unbalanced '\\\('" } */
/* { dg-error "unbalanced '\\\)'" "" { target *-*-* } .-1 } */
/* { dg-error "unbalanced '\\\{'" "" { target *-*-* } .-2 } */
/* { dg-error "unbalanced '\\\}'" "" { target *-*-* } .-3 } */
#embed __FILE__ limit(1) prefix(]) /* { dg-error "unbalanced '\\\]'" } */
#embed __FILE__ limit(1) prefix([) /* { dg-error "unbalanced '\\\)'" } */
/* { dg-error "unbalanced '\\\['" "" { target *-*-* } .-1 } */
/* { dg-error "unbalanced '\\\('" "" { target *-*-* } .-2 } */
#embed __FILE__ limit(1) __suffix__(() /* { dg-error "unbalanced '\\\('" } */
#embed __FILE__ limit(1) __suffix__(({[})]) /* { dg-error "unbalanced '\\\('" } */
/* { dg-error "unbalanced '\\\)'" "" { target *-*-* } .-1 } */
/* { dg-error "unbalanced '\\\{'" "" { target *-*-* } .-2 } */
/* { dg-error "unbalanced '\\\}'" "" { target *-*-* } .-3 } */
#embed __FILE__ limit(1) suffix([) /* { dg-error "unbalanced '\\\)'" } */
/* { dg-error "unbalanced '\\\['" "" { target *-*-* } .-1 } */
/* { dg-error "unbalanced '\\\('" "" { target *-*-* } .-2 } */
#embed __FILE__ limit(1) if_empty(() /* { dg-error "unbalanced '\\\('" } */
#embed __FILE__ limit(1) __if_empty__(({[})]) /* { dg-error "unbalanced '\\\('" } */
/* { dg-error "unbalanced '\\\)'" "" { target *-*-* } .-1 } */
/* { dg-error "unbalanced '\\\{'" "" { target *-*-* } .-2 } */
/* { dg-error "unbalanced '\\\}'" "" { target *-*-* } .-3 } */
#embed __FILE__ limit(1) __if_empty__([) /* { dg-error "unbalanced '\\\)'" } */
/* { dg-error "unbalanced '\\\['" "" { target *-*-* } .-1 } */
/* { dg-error "unbalanced '\\\('" "" { target *-*-* } .-2 } */
#embed __FILE__ limit(1) gnu::non_existent_parameter(() /* { dg-error "unknown embed parameter 'gnu::non_existent_parameter'" } */
/* { dg-error "unbalanced '\\\('" "" { target *-*-* } .-1 } */
#embed __FILE__ limit(1) __gnu__::__non_existent_parameter__(({[})]) /* { dg-error "unknown embed parameter 'gnu::non_existent_parameter'" } */
/* { dg-error "unbalanced '\\\('" "" { target *-*-* } .-1 } */
/* { dg-error "unbalanced '\\\)'" "" { target *-*-* } .-2 } */
/* { dg-error "unbalanced '\\\{'" "" { target *-*-* } .-3 } */
/* { dg-error "unbalanced '\\\}'" "" { target *-*-* } .-4 } */
#embed __FILE__ limit(1) __gnu__::__non_existent_parameter__([) /* { dg-error "unknown embed parameter 'gnu::non_existent_parameter'" } */
/* { dg-error "unbalanced '\\\)'" "" { target *-*-* } .-1 } */
/* { dg-error "unbalanced '\\\['" "" { target *-*-* } .-2 } */
/* { dg-error "unbalanced '\\\('" "" { target *-*-* } .-3 } */
#embed limit(1) /* { dg-error "'#embed' expects '\\\"FILENAME\\\"' or '<FILENAME>'" } */
#define FOO 1
#embed __FILE__ limit(0 + defined(FOO)) /* { dg-error "'defined' in '#embed' parameter" } */
#embed /* { dg-error "'#embed' expects '\\\"FILENAME\\\"' or '<FILENAME>'" } */
#embed "
/* { dg-warning "missing terminating \\\" character" "" { target *-*-* } .-1 } */
 /* { dg-error "'#embed' expects '\\\"FILENAME\\\"' or '<FILENAME>'" "" { target *-*-* } .-2 } */
#embed <
/* { dg-error "empty filename in #embed" "" { target *-*-* } .-1 } */
/* { dg-error "missing terminating '>' character" "" { target *-*-* } .-2 } */
#embed >  /* { dg-error "'#embed' expects '\\\"FILENAME\\\"' or '<FILENAME>'" } */
#embed "" /* { dg-error "empty filename in #embed" } */
#embed <> /* { dg-error "empty filename in #embed" } */
#embed embed-4.c  /* { dg-error "'#embed' expects '\\\"FILENAME\\\"' or '<FILENAME>'" } */
#embed __FILE__ foo: /* { dg-error "expected parameter name" } */
/* { dg-error "unknown embed parameter 'foo'" "" { target *-*-* } .-1 } */
#embed __FILE__ bar:: /* { dg-error "expected parameter name" } */
#embed __FILE__ :: /* { dg-error "expected parameter name" } */
#embed __FILE__ foo:bar /* { dg-error "expected parameter name" } */
/* { dg-error "unknown embed parameter 'foo'" "" { target *-*-* } .-1 } */
#embed __FILE__ foo::bar::baz /* { dg-error "expected parameter name" } */
/* { dg-error "unknown embed parameter 'foo::bar'" "" { target *-*-* } .-1 } */
#embed __FILE__ foo : : bar /* { dg-error "expected parameter name" } */
/* { dg-error "unknown embed parameter 'foo'" "" { target *-*-* } .-1 } */
#embed __FILE__ 42 /* { dg-error "expected parameter name" } */
#embed __FILE__ 42::foo /* { dg-error "expected parameter name" } */
#embed __FILE__ foo::42 /* { dg-error "expected parameter name" } */
#embed __FILE__ limit(1/0) /* { dg-error "division by zero in #embed" } */
