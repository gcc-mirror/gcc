/* { dg-do compile } */
/* { dg-additional-options "-std=c23 -mcpu=xiangshan-kunminghu" } */

_Float16 f, g;
void foo() { f /= g; }
