/* Test btf_type_tag attribute warnings.  */
/* { dg-do compile } */

int __attribute__((btf_type_tag ("A"))) a (int x); /* { dg-warning "does not apply to functions" } */

__attribute__((btf_type_tag ("B"))) int *b (int y); /* { dg-warning "does not apply to functions" } */

int *c (int z) __attribute__((btf_type_tag ("C"))); /* { dg-warning "does not apply to functions" } */
