// PR117013
/* { dg-do run } */
/* { dg-options "-O2 -std=c++20 -save-temps -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" ""} } */

#include <compare>
#include <stdint.h>

/*  Some implementation-defined value (other than 2) to represent
    partial_ordering::unordered (that for libc++ in this case). */
#define IMP_UN  -127

#define SPACESHIP_FN(TYPE)              \
    [[gnu::noipa]]                      \
    auto ss_##TYPE (TYPE a, TYPE b)     \
    { return a <=> b; }                 \

#define SPACESHIP_FN_NN(TYPE)                                \
    [[gnu::noipa, gnu::optimize ("-ffinite-math-only")]]     \
    auto ss_##TYPE##_no_nans (TYPE a, TYPE b)                \
    { return a <=> b; }                                      \

/*  <=> implementation for floating-point operands. */
#define SPACESHIP_FP_IDIOM(TYPE)                                              \
    [[gnu::noipa]]                                                            \
    int ss_##TYPE##_idiom (TYPE a, TYPE b)                                    \
    { return ((a) == (b) ? 0 : (a) < (b) ? -1 : (a) > (b) ? 1 : IMP_UN); }    \

#define RUN_TEST(TYPE, ARGA, ARGB, EXPECT, SUFF)            \
    if (ss_##TYPE##SUFF ((ARGA), (ARGB)) != (EXPECT))       \
        __builtin_abort();                                  \

/*
** _Z8ss_floatff:
**	fcmpe	s0, s1
**	csinv	(w[0-9]+), wzr, wzr, pl
**	cset	(w[0-9]+), vs
**	csinc	w0, \1, \2, ls
**	ret
*/
SPACESHIP_FN(float);

/*
** _Z16ss_float_no_nansff:
**	fcmpe	s0, s1
**	csinv	(w[0-9]+), wzr, wzr, pl
**	csinc	w0, \1, wzr, ls
**	ret
*/
SPACESHIP_FN_NN(float);

/*
** _Z9ss_doubledd:
**	fcmpe	d0, d1
**	csinv	(w[0-9]+), wzr, wzr, pl
**	cset	(w[0-9]+), vs
**	csinc	w0, \1, \2, ls
**	ret
*/
SPACESHIP_FN(double);

/*
** _Z17ss_double_no_nansdd:
**	fcmpe	d0, d1
**	csinv	(w[0-9]+), wzr, wzr, pl
**	csinc	w0, \1, wzr, ls
**	ret
*/
SPACESHIP_FN_NN(double);

/*
** _Z14ss_float_idiomff:
**	fcmpe	s0, s1
**	csinv	(w[0-9]+), wzr, wzr, pl
**	mov	(w[0-9]+), -128
**	csel	(w[0-9]+), \2, wzr, vs
**	csinc	w0, \1, \3, ls
**	ret
*/
SPACESHIP_FP_IDIOM(float);

/*
** _Z15ss_double_idiomdd:
**	fcmpe	d0, d1
**	csinv	(w[0-9]+), wzr, wzr, pl
**	mov	(w[0-9]+), -128
**	csel	(w[0-9]+), \2, wzr, vs
**	csinc	w0, \1, \3, ls
**	ret
*/
SPACESHIP_FP_IDIOM(double);

/*
** _Z10ss_int32_tii:
**	cmp	w0, w1
**	cset	(w[0-9]+), gt
**	csinv	w0, \1, wzr, ge
**	ret
*/
SPACESHIP_FN(int32_t);

/*
** _Z10ss_int64_tll:
**	cmp	x0, x1
**	cset	(w[0-9]+), gt
**	csinv	w0, \1, wzr, ge
**	ret
*/
SPACESHIP_FN(int64_t);

/*
** _Z11ss_uint32_tjj:
**	cmp	w0, w1
**	cset	(w[0-9]+), hi
**	csinv	w0, \1, wzr, cs
**	ret
*/
SPACESHIP_FN(uint32_t);

/*
** _Z11ss_uint64_tmm:
**	cmp	x0, x1
**	cset	(w[0-9]+), hi
**	csinv	w0, \1, wzr, cs
**	ret
*/
SPACESHIP_FN(uint64_t);


int
main()
{
    /* Single precision floating point.  */
    RUN_TEST (float, -1.0f, 1.0f, std::partial_ordering::less,);
    RUN_TEST (float, -1.0f, 1.0f, -1, _idiom);

    RUN_TEST (float, 1.0f, -1.0f, std::partial_ordering::greater,);
    RUN_TEST (float, 1.0f, -1.0f, 1, _idiom);

    RUN_TEST (float, -1.0f, -1.0f, std::partial_ordering::equivalent,);
    RUN_TEST (float, -1.0f, -1.0f, 0, _idiom);

    RUN_TEST (float, __builtin_nanf(""), 1.0f, std::partial_ordering::unordered,);
    RUN_TEST (float, __builtin_nanf(""), 1.0f, IMP_UN, _idiom);
    RUN_TEST (float, 1.0f ,__builtin_nanf(""), std::partial_ordering::unordered,);
    RUN_TEST (float, 1.0f, __builtin_nanf(""), IMP_UN, _idiom);

    /* No-NaNs.  */
    RUN_TEST (float, -1.0f, 1.0f, std::partial_ordering::less, _no_nans);
    RUN_TEST (float, 1.0f, -1.0f, std::partial_ordering::greater, _no_nans);
    RUN_TEST (float, -1.0f, -1.0f, std::partial_ordering::equivalent, _no_nans);

    /* Double precision floating point.  */
    RUN_TEST (double, -1.0f, 1.0f, std::partial_ordering::less,);
    RUN_TEST (double, -1.0f, 1.0f, -1, _idiom);

    RUN_TEST (double, 1.0f, -1.0f, std::partial_ordering::greater,);
    RUN_TEST (double, 1.0f, -1.0f, 1, _idiom);

    RUN_TEST (double, -1.0f, -1.0f, std::partial_ordering::equivalent,);
    RUN_TEST (double, -1.0f, -1.0f, 0, _idiom);

    RUN_TEST (double, __builtin_nanf(""), 1.0f, std::partial_ordering::unordered,);
    RUN_TEST (double, __builtin_nanf(""), 1.0f, IMP_UN, _idiom);
    RUN_TEST (double, 1.0f, __builtin_nanf(""), std::partial_ordering::unordered,);
    RUN_TEST (double, 1.0f, __builtin_nanf(""), IMP_UN, _idiom);

    /* No-NaNs.  */
    RUN_TEST (double, -1.0f, 1.0f, std::partial_ordering::less, _no_nans);
    RUN_TEST (double, 1.0f, -1.0f, std::partial_ordering::greater, _no_nans);
    RUN_TEST (double, -1.0f, -1.0f, std::partial_ordering::equivalent, _no_nans);

    /* Single integer.  */
    RUN_TEST (int32_t, -42, 0, std::strong_ordering::less,);
    RUN_TEST (int32_t, 0, -42, std::strong_ordering::greater,);
    RUN_TEST (int32_t, 42, 42, std::strong_ordering::equal,);

    RUN_TEST (uint32_t, 0, 42, std::strong_ordering::less,);
    RUN_TEST (uint32_t, 42, 0, std::strong_ordering::greater,);
    RUN_TEST (uint32_t, 42, 42, std::strong_ordering::equal,);

    /* Double integer.  */
    RUN_TEST (int64_t, -42, 0, std::strong_ordering::less,);
    RUN_TEST (int64_t, 42, 0, std::strong_ordering::greater,);
    RUN_TEST (int64_t, 42, 42, std::strong_ordering::equal,);

    RUN_TEST (uint64_t, 0, 42, std::strong_ordering::less,);
    RUN_TEST (uint64_t, 42, 0, std::strong_ordering::greater,);
    RUN_TEST (uint64_t, 42, 42, std::strong_ordering::equal,);

    return 0;
}