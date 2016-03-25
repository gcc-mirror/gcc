/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

# 9 "somefile" 2 /* { dg-warning "linemarker ignored due to incorrect nesting" } */
not_a_type a; /* { dg-error "unknown type" } */
