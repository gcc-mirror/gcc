/* { dg-do compile } */
/* { dg-options "-std=c89 -pedantic" } */
struct { int a[]; } x = { 0 };	/* { dg-error "(flexible array member)|(near initialization)" } */
