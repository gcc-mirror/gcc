/* Tests of duplication.  */
/* { dg-do compile } */

@interface class1
- (int) meth1;
- (void) meth1;  /* { dg-error "duplicate declaration of instance method" } */
@end

@interface class2
+ (void) meth1;
+ (int) meth1;  /* { dg-error "duplicate declaration of class method" } */
@end

@interface class3
- (int) meth1;
@end

@implementation class3
- (int) meth1 { return 0; }
- (int) meth1 { return 0; }  /* { dg-error "duplicate definition of instance method" } */
/* { dg-error "redefinition of" "" { target *-*-* } 20 } */
/* { dg-error "previously defined here" "" { target *-*-* } 19 } */
@end

@interface class4
+ (void) meth1;
@end

@implementation class4
+ (void) meth1 {}
+ (void) meth1 {}  /* { dg-error "duplicate definition of class method" } */
/* { dg-error "redefinition of" "" { target *-*-* } 31 } */
/* { dg-error "previously defined here" "" { target *-*-* } 30 } */
@end
