/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Test the char8_t promotion rules. */
/* { dg-do compile { target c++11 } } */
/* { dg-options "-fchar8_t -fsigned-char -Wall -Wconversion -Wsign-conversion -Wsign-promo" } */

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

void m(char8_t c)
{
    f_c (c);	/* { dg-warning "change the sign" } */
    fsc (c);	/* { dg-warning "change the sign" } */
    fuc (c);
    f_s (c);
    fss (c);
    fus (c);
    f_i (c);
    fsi (c);
    fui (c);
    f_l (c);
    fsl (c);
    ful (c);
    f_ll (c);
    fsll (c);
    full (c);
}
