/* Testing conditional warnings (without headers).  */
/* Author: David Ayers */

/* { dg-do compile } */

#define nil ((id)0)
@interface MyObject
@end

@protocol MyProtocol
@end

@interface MyProtoObject <MyProtocol>
@end


int
main (int argc, char *argv[])
{
  id var_id = nil;
  id <MyProtocol> var_id_p = nil;
  MyObject *var_obj = nil;
  MyProtoObject *var_obj_p = nil;

  var_id = (var_id == var_obj) ? var_id : var_obj;
  var_id = (var_id == var_obj) ? var_id : var_obj_p;

  /* Ayers: Currently, the following test case passes for
     technically the wrong reason (see below).
  */
  var_obj_p = (var_id == var_obj) ? var_obj_p : var_obj; /* { dg-warning "distinct Objective-C types" } */
  var_obj_p = (var_id == var_obj) ? var_obj_p : var_id_p;

  /* Ayers: The first of the following test cases
     should probably warn for var_obj_p = var_obj,
     yet that would require extensive changes to
     build_conditional_expr to create a tree with
     multiple types that the assignment would have
     to evaluate both versions for correct diagnostics.
  */
  var_obj_p = (var_id == var_obj) ? var_id : var_obj;  
  var_obj_p = (var_id == var_obj) ? var_id : var_obj_p;

  return 0;
}
