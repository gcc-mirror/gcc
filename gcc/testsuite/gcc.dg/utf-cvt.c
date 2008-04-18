/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Test the char16_t and char32_t promotion rules. */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wall -Wconversion -Wsign-conversion" } */

typedef unsigned short	char16_t;
typedef unsigned int	char32_t;

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

void m (char16_t c0, char32_t c1)
{
    f_c (c0);				/* { dg-warning "alter its value" } */
    fsc (c0);				/* { dg-warning "alter its value" } */
    fuc (c0);				/* { dg-warning "alter its value" } */
    f_s (c0);				/* { dg-warning "change the sign" } */
    fss (c0);				/* { dg-warning "change the sign" } */
    fus (c0);
    f_i (c0);
    fsi (c0);
    fui (c0);
    f_l (c0);
    fsl (c0);
    ful (c0);

    f_c (c1);				/* { dg-warning "alter its value" } */
    fsc (c1);				/* { dg-warning "alter its value" } */
    fuc (c1);				/* { dg-warning "alter its value" } */
    f_s (c1);				/* { dg-warning "alter its value" } */
    fss (c1);				/* { dg-warning "alter its value" } */
    fus (c1);				/* { dg-warning "alter its value" } */
    f_i (c1);				/* { dg-warning "change the sign" } */
    fsi (c1);				/* { dg-warning "change the sign" } */
    fui (c1);
    f_l (c1);				/* { dg-warning "change the sign" } */
    fsl (c1);				/* { dg-warning "change the sign" } */
    ful (c1);
}
