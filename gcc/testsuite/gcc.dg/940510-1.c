/* { dg-do compile } */
/* { dg-options "-std=c89 -pedantic" } */
struct { int a[]; } x = { 0 };	/* { dg-warning "ISO C90 does not support flexible array members" } */
/* { dg-error "flexible array member in a struct with no named members"  "" { target *-*-* }  3 } */

