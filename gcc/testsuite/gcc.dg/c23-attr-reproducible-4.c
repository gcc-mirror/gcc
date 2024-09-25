/* Test C23 reproducible attribute: duplicates.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

int a () [[reproducible, __reproducible__]];
int b () [[__reproducible__, reproducible]];
int c () [[reproducible, reproducible]];
int d () [[__reproducible__, __reproducible__]];
int d () [[reproducible]];
int d () [[__reproducible__]];
[[reproducible, reproducible]];
/* { dg-error "ignored" "ignored" { target *-*-* } .-1 } */
