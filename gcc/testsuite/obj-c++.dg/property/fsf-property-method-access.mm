/* test access in methods, auto-generated getter/setter based on property name.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#ifdef __cplusplus
extern "C" {
#endif
extern int printf (const char *fmt,...) ;
extern void abort (void);

typedef struct objc_class *Class;

#ifdef __NEXT_RUNTIME__

extern id class_createInstance(Class, long);
#define class_create_instance(C) class_createInstance(C, 0)

#else

extern id class_create_instance(Class);

#endif
#ifdef __cplusplus
}
#endif

@interface Bar
{
@public
#ifdef __NEXT_RUNTIME__
  Class isa;
#else
  Class class_pointer;
#endif
}
+ (id) initialize;
+ (id) alloc ;
- (id) init;

- (int) lookAtProperty;
- (void) setProperty: (int) v;

@property int FooBar;
@end

@implementation Bar

+initialize { return self;}
+ (id) alloc { return class_create_instance(self);}

- (id) init {return self;}

@property int FooBar;

- (int) lookAtProperty { return FooBar; }
- (void) setProperty: (int) v { FooBar = v; }

@end

int main(int argc, char *argv[]) {
  int res;
  Bar *f = [[Bar alloc] init];

  /* First, establish that the property getter & setter have been synthesized 
     and operate correctly.  */
  [f setProperty:11];

  if (f->_FooBar != 11)
    { printf ("setProperty did not set _FooBar\n"); abort ();}
      
  res = [f lookAtProperty];    
  if (res != 11 )
    { printf ("[f lookAtProperty] = %d\n",  res); abort ();}
  
  /* Make sure we haven't messed up the shortcut form.  */
  /* read ... */
  res = f.FooBar;
  if (res != 11 )
    { printf ("f.FooBar = %d\n",  res); abort ();}
  
  /* ... write. */
  f.FooBar = 0;
  /* printf ("seems OK\n",  res); */
  return f.FooBar;
}

