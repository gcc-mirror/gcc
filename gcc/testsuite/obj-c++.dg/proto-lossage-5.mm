/* Do not lose references to forward-declared protocols.  */
/* { dg-do compile } */
@class MyBaseClass;
@class MyClassThatFails;
@protocol _MyProtocol;

@interface MyClassThatFails
- (MyBaseClass<_MyProtocol> *) aMethod;
@end

@interface MyBaseClass
@end

@protocol _MyProtocol
@end

@implementation MyClassThatFails
- (MyBaseClass<_MyProtocol> *) aMethod
{
    return 0;
}
@end
