/* Test C23 unsequenced attribute: duplicates.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

int a () [[unsequenced, __unsequenced__]];
int b () [[__unsequenced__, unsequenced]];
int c () [[unsequenced, unsequenced]];
int d () [[__unsequenced__, __unsequenced__]];
int d () [[unsequenced]];
int d () [[__unsequenced__]];
[[unsequenced, unsequenced]];
/* { dg-error "ignored" "ignored" { target *-*-* } .-1 } */
