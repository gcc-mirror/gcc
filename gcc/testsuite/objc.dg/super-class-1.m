/* Test super classes.  */
/* { dg-do compile } */

@interface class0 : supclass0
@end  /* { dg-error "Cannot find interface declaration for .*, superclass" } */
