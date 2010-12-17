/* Test if addition of 'volatile' to object causes bogus error in presence of try-catch. */
/* { dg-options "-fobjc-exceptions" } */
/* { dg-do compile } */

@interface Exception
@end

class CppObj {
public:
    void constMethod() const {
    }
};

@interface MyObject : Exception
- (void)doSomething;
- (void)myMethod;
@end

@implementation MyObject
- (void)doSomething {
}

- (void)myMethod {
    CppObj cppObj;
    
    @try {
        [self doSomething];
    }
    @catch (Exception *exception) {
    }
    
    cppObj.constMethod();
}
@end
