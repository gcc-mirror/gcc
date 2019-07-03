/* Contributed by Iain Sandoe <iain@sandoe.co.uk>, May 2019.  */
/* { dg-do compile } */

/* Basic check of parsing instancetype.  */

extern id class_createInstance (id, int);
extern id class_getSuperclass (id);

@interface MyObject
{
  Class isa;
}
+ (instancetype)alloc;
- (instancetype)init;
+ (instancetype)initialize;
+ (instancetype)factoryMethodA;
+ (id)factoryMethodB;
+ (Class) class;
+ (Class) superclass;
@end

@implementation MyObject
+ (instancetype)alloc { return class_createInstance (self, 0); }
- (instancetype)init  { return self; }
+ (instancetype)initialize { return self; }
+ (instancetype)factoryMethodA { return [[[self class] alloc] init]; }
+ (id)factoryMethodB { return [[[self class] alloc] init]; }
+ (Class) class { return self; }
+ (Class) superclass { return class_getSuperclass (self); }
@end
