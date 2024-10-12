/* P2071R2 - Named universal character escapes */
/* { dg-do compile } */
/* { dg-require-effective-target wchar } */
/* { dg-options "-std=gnu99 -Wno-c++-compat" { target c } } */
/* { dg-options "-std=c++23" { target c++ } } */

#ifndef __cplusplus
typedef __CHAR32_TYPE__ char32_t;
#endif

const char32_t *a = U"\N{ZERO WIDTH NO BREAK SPACE}";		/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{ZERO WIDTH NO-BREAK SPACE\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *b = U"\N{giraffe face}";			/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{GIRAFFE FACE\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *c = U"\N{Giraffe Face}";			/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{GIRAFFE FACE\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *d = U"\N{   GiRaFfE_fAcE__ ___}";		/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{GIRAFFE FACE\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *e = U"\N{GIRAFFE}";				/* { dg-error "is not a valid universal character" } */
const char32_t *f = U"\N{Hangul_Syllable_gAgg_}";		/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{HANGUL SYLLABLE GAGG\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *g = U"\N{HANGUL SYLLABLE gagg}";		/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{HANGUL SYLLABLE GAGG\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *h = U"\N{HANGULSYLLABLEGAGG}";			/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{HANGUL SYLLABLE GAGG\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *i = U"\N{HANGUL_SYLLABLE_GAGG}";		/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{HANGUL SYLLABLE GAGG\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *j = U"\N{HANGUL SYLLABLE }";			/* { dg-error "is not a valid universal character" } */
const char32_t *k = U"\N{CJK-COMPATIBILITY-IDEOGRAPH-2F801}";	/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{CJK COMPATIBILITY IDEOGRAPH-2F801\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *l = U"\N{CjK_COMPATIBILITY IDEOGRAPH 2f801}";	/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{CJK COMPATIBILITY IDEOGRAPH-2F801\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *m = U"\N{CjK_COMPATIBILITY IDEOGRAPH 2f80}";	/* { dg-error "is not a valid universal character" } */
const char32_t *n = U"\N{CJK COMPATIBILITY IDEOGRAPH-}";	/* { dg-error "is not a valid universal character" } */
const char32_t *o = U"\N{CJK COMPATIBILITY IDEOGRAPH-X}";	/* { dg-error "is not a valid universal character" } */
const char32_t *p = U"\N{Tibetan Letter A}";			/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{TIBETAN LETTER A\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *q = U"\N{Tibetan LetterA}";			/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{TIBETAN LETTER A\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *r = U"\N{Tibetan Letter-A}";			/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{TIBETAN LETTER A\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *s = U"\N{Tibetan Letter -A}";			/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{TIBETAN LETTER -A\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *t = U"\N{TibetanLetter  -A}";			/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{TIBETAN LETTER -A\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *u = U"\N{Hangul Jungseong oe}";			/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{HANGUL JUNGSEONG OE\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *v = U"\N{Hangul Jungseong o- e}";		/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{HANGUL JUNGSEONG O-E\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *w = U"\N{HangulJungseongo-e}";			/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{HANGUL JUNGSEONG O-E\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *x = U"\N{Hangul Jungseong oe          __   }";	/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{HANGUL JUNGSEONG OE\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *y = U"\N{Hangul Jungseong o- e     __      }";	/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{HANGUL JUNGSEONG O-E\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *z = U"\N{Hangul Jungseong o -e}";		/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{HANGUL JUNGSEONG O-E\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *A = U"\N{Hangul Jungseong o -e     __      }";	/* { dg-error "is not a valid universal character" } */
								/* { dg-message "did you mean '\\\\N\\{HANGUL JUNGSEONG O-E\\}'\\?" "" { target *-*-* } .-1 } */
const char32_t *B = U"\N{O}";					/* { dg-error "is not a valid universal character" } */
