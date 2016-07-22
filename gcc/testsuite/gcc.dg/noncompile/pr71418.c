/* PR c/71418 */
/* { dg-do compile } */

_Alignas (int) int a[7++]; /* { dg-error "lvalue required" } */
