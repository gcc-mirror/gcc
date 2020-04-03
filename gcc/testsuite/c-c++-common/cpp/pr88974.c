/* PR preprocessor/88974 */
/* { dg-do preprocess } */

#if __has_include (<pr88974.h)
/* { dg-error "missing terminating > character" "" { target *-*-* } .-1 } */
/* { dg-error "missing '\\\)' after .__has_include. operand" "" { target *-*-* } .-2 } */
#endif
