/* { dg-do compile } */
/* { dg-require-effective-target strub } */
/* { dg-options "-fstrub=all -O2" } */

void rb_ec_error_print(struct rb_execution_context_struct *volatile) {}	/* { dg-warning "declared inside parameter list" } */
