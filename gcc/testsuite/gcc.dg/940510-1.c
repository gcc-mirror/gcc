/* { dg-do compile } */
/* { dg-options "-std=c89 -pedantic" } */
struct { int a[]; } x = { 0 };	/* { dg-warning "ISO C90 does not support flexible array members" } */
/* { dg-warning "flexible array member in a struct with no named members is a GCC extension"  "" { target *-*-* }  .-1 } */
/* { dg-warning "initialization of a flexible array member"  "" { target *-*-* }  .-2 } */
