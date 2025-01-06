/* { dg-do compile } */
/* { dg-options "-std=c23" } */

void convert(struct fractpoint *pt);	/* { dg-warning "declared inside parameter list" } */

