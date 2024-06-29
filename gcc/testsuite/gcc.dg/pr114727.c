/* { dg-do compile }
 * { dg-options "-std=c23 -g" } */

#define Y [[gnu::aligned(128)]]
extern struct Y foo { int x; } x;
struct foo { int x; };		/* { dg-error "redefinition" } */
