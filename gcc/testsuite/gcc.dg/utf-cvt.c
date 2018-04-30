/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Test the char16_t and char32_t promotion rules. */
/* { dg-do compile } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-std=gnu99 -Wall -Wconversion -Wsign-conversion" } */

typedef __CHAR16_TYPE__ char16_t;
typedef __CHAR32_TYPE__ char32_t;

extern void f_c (char);
extern void fsc (signed char);
extern void fuc (unsigned char);
extern void f_s (short);
extern void fss (signed short);
extern void fus (unsigned short);
extern void f_i (int);
extern void fsi (signed int);
extern void fui (unsigned int);
extern void f_l (long);
extern void fsl (signed long);
extern void ful (unsigned long);
extern void f_ll (long long);
extern void fsll (signed long long);
extern void full (unsigned long long);

void m (char16_t c0, char32_t c1)
{
    f_c (c0);	/* { dg-warning "conversion from .char16_t\[^\n\r\]*. to .char. may change value" } */
    fsc (c0);	/* { dg-warning "may change value" } */
    fuc (c0);	/* { dg-warning "may change value" } */
    f_s (c0);	/* { dg-warning "change the sign" } */
    fss (c0);	/* { dg-warning "change the sign" } */
    fus (c0);
    f_i (c0);
    fsi (c0);
    fui (c0);
    f_l (c0);
    fsl (c0);
    ful (c0);
    f_ll (c0);
    fsll (c0);
    full (c0);

    f_c (c1);	/* { dg-warning "may change value" } */
    fsc (c1);	/* { dg-warning "may change value" } */
    fuc (c1);	/* { dg-warning "may change value" } */
    f_s (c1);	/* { dg-warning "may change value" } */
    fss (c1);	/* { dg-warning "may change value" } */
    fus (c1);	/* { dg-warning "may change value" } */
    f_i (c1);	/* { dg-warning "change the sign" "" { target { ! int16 } } } */
    fsi (c1);	/* { dg-warning "change the sign" "" { target { ! int16 } } } */
    fui (c1);
    f_l (c1);	/* { dg-warning "change the sign" "" { target { llp64 || ilp32 } } } */
    fsl (c1);	/* { dg-warning "change the sign" "" { target { llp64 || ilp32 } } } */
    ful (c1);
    f_ll (c1);
    fsll (c1);
    full (c1);
}
