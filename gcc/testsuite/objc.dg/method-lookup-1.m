/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, June 2011.  */
/* { dg-do compile } */

@class NotKnown;

@protocol MyProtocol
+ (id) classMethod;
- (id) instanceMethod;
@end

@protocol MyProtocol2
+ (id) classMethod2;
- (id) instanceMethod2;
@end

void test (Class x, Class <MyProtocol> y, id w, id <MyProtocol> z, NotKnown *a, NotKnown <MyProtocol> *b)
{
  /* "Class x" means that "x" responds to any class methods, and may
     also respond to instance methods because instance methods of the
     root class are class methods.  */
  [x classMethod]; /* No warning here.  */

  [x instanceMethod]; /* No warning here.  */


  /* "Class <MyProtocol> y" means that "y" responds to any class
     methods specified in the protocol MyProtocol, but not to other
     class or instance methods.  If a class method is not found, an
     instance method from the protocol may be used instead but that is
     suspicious and gets a warning.  */
  [y classMethod]; /* No warning here.  */

  [y instanceMethod]; /* { dg-warning "found .\\-instanceMethod. instead of .\\+instanceMethod. in protocol" } */

  [y classMethod2]; /* { dg-warning ".\\+classMethod2. not found in protocol" } */

  [y instanceMethod2]; /* { dg-warning ".\\+instanceMethod2. not found in protocol" } */


  /* If a class is specified by name, the @interface must be available
     to check what it responds to.  */
  [NotKnown classMethod]; /* { dg-warning ".interface of class .NotKnown. not found" } */


  /* "id w" means that "w" responds to anything, both class and
     instance methods.  */
  [w instanceMethod]; /* No warning here.  */

  [w instanceMethod2]; /* No warning here.  */

  [w classMethod]; /* No warning here.  */

  [w classMethod2]; /* No warning here.  */


  /* "id <MyProtocol> z" means that "z" responds to any instance
     methods in the protocol, but not class methods.  To select class
     methods, you use "Class <MyProtocol> z".  */
  [z instanceMethod]; /* No warning here.  */

  [z instanceMethod2]; /* { dg-warning ".\\-instanceMethod2. not found in protocol" } */

  [z classMethod];     /* { dg-warning ".\\-classMethod. not found in protocol" } */

  [z classMethod2];    /* { dg-warning ".\\-classMethod2. not found in protocol" } */


  /* "NotKnown *a" means that "a" is an instance of NotKnown.  Since
     the programmer explicitly specified the class name, it must be
     because they expect the compiler to do type-checking; the
     @interface must be available to do this check, otherwise the
     compiler does not know what "a" responds to.  */
  [a instanceMethod];  /* { dg-warning ".interface of class .NotKnown. not found" } */

  /* But, if you cast it to "id", then you're disabling type-checking
     and the warnings should go away.  */
  [(id)a instanceMethod]; /* No warning here.  */


  /* "NotKnown <MyProtocol> *b" means that "a" is an instance of
     NotKnown, and also implements protocol <MyProtocol>.  If you send
     a message that is part of the protocol, then the compiler can do
     type-checking and all is fine.  */
  [b instanceMethod];

  /* But if you send a message that is not part of the protocol, then
     you'll get a warning that the method can not be found in the
     protocol.  */
  [b instanceMethod2]; /* { dg-warning ".\\-instanceMethod2. not found in protocol" } */ 

  /* But, if you cast it to "id", then you're disabling type-checking
     and the warnings should go away.  */
  [(id)b instanceMethod2]; /* No warning here.  */
}
