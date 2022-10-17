/* PR c/103587 */
/* { dg-do compile } */
/* { dg-options "" } */

[[foo::bar(
#pragma GCC ivdep
)]];	/* { dg-warning "attribute ignored" } */
