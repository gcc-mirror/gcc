/* { dg-do compile } */

/* All these C++ keywords are acceptable in ObjC method names, hence
   should be accepted for property getters and setters.  */

@interface Test
{
  Class isa;
}
@property (getter=namespace) int p0;
@property (setter=namespace:) int p1;
@property (getter=and) int p2;
@property (setter=and:) int p3;
@property (getter=class) int p4;
@property (setter=class:) int p5;
@property (getter=new) int p6;
@property (setter=new:) int p7;
@property (getter=delete) int p8;
@property (setter=delete:) int p9;
@property (getter=delete) int p10;
@property (setter=delete:) int p11;
@end
