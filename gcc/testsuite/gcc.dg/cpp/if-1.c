/* { dg-do preprocess } */ 
/* { dg-options "-pedantic-errors" } */

#if 0xa != 10
#error 0xa != 10 /* { dg-bogus "#error" "normal conversion" } */
#endif

#if 077 != 63
#error 077 != 63 /* { dg-bogus "#error" "normal conversion" } */
#endif

#if 12wrt /* { dg-error "invalid suffix" "invalid number" } */
#endif

#if 0abc /* { dg-error "invalid suffix" "invalid number" } */
#endif

#if 42abc /* { dg-error "invalid suffix" "invalid number" } */
#endif

#if 0xabc != 2748
#error 0xabc	/* { dg-bogus "#error" "normal conversion" } */
#endif

#if 1.2 /* { dg-error "loating point numbers" "floating point in #if" } */
#endif

#if 4uu /* { dg-error "invalid suffix" "too many suffixes" } */
#endif

#if 124123231lll /* { dg-error "invalid suffix" "too many suffixes" } */
#endif

#if 1234lul	/* { dg-error "invalid suffix" "u between ls" } */
#endif

#if 099 /* { dg-error "digits beyond the radix" "decimal in octal constant" } */
#endif

#if 0xfffffffffffffffff /* { dg-error "integer constant out of range" "range error" } */
#endif
