/* PR c/69507 - bogus warning: ISO C does not allow '__alignof__ (expression)'
 */
/* { dg-do compile } */
/* { dg-options "-std=c11 -Wno-error -Wpedantic" } */

extern int e;

int a[] = {
    __alignof__ (e),
    _Alignof (e)       /* { dg-warning "ISO C does not allow ._Alignof \\(expression\\)." } */
};
