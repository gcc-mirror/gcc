/* Test alias warnings.  */
/* { dg-do compile } */

@compatibility_alias class1 class2; /* { dg-warning "Cannot find class" } */

@interface class3;
@end

@interface class4;
@end

@compatibility_alias class4 class3;  /* { dg-warning "Class" "already exists" } */
