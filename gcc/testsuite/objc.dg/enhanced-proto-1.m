/* APPLE LOCAL file C* language */
/* Test use of @optional/@required keywords in @protocol class. */
/* { dg-do compile } */

@protocol MyProto1 
@optional
- (void) FOO;
@optional
- (void) FOO;
@required 
- (void) REQ;
@optional
@end

@protocol  MyProto2 <MyProto1>
- (void) FOO2;
@optional
- (void) FOO3;
@end
