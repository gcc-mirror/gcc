/* PR c/29467 */
/* { dg-do compile } */
/* { dg-options "-std=c89 -Wpedantic" } */

_Bool b; /* { dg-warning "ISO C90 does not support boolean types" } */
typedef _Bool B; /* { dg-warning "ISO C90 does not support boolean types" } */
static _Bool sb; /* { dg-warning "ISO C90 does not support boolean types" } */

_Bool /* { dg-warning "ISO C90 does not support boolean types" } */
foo (_Bool bp) /* { dg-warning "ISO C90 does not support boolean types" } */
{
  _Bool bl; /* { dg-warning "ISO C90 does not support boolean types" } */
}
