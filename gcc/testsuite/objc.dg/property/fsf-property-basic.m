/* Basic test, auto-generated getter/setter based on property name.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

extern int printf (char *fmt,...) ;
extern void abort (void);

typedef struct objc_class *Class;

#ifdef __NEXT_RUNTIME__

extern id class_createInstance(Class, long);
#define class_create_instance(C) class_createInstance(C, 0)

#else

extern id class_create_instance(Class);

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

@property int FooBar;
@end

@implementation Bar

+initialize { return self;}
+ (id) alloc { return class_create_instance(self);}

- (id) init {return self;}

@property int FooBar ;
@end

int main(int argc, char *argv[]) {
  int res;
  Bar *f = [[Bar alloc] init];

  /* First, establish that the property getter & setter have been synthesized 
     and operate correctly.  */
  [f setFooBar:1];

  if (f->_FooBar != 1)
    { printf ("setFooBar did not set _FooBar\n"); abort ();}
      
  res = [f FooBar];
    
  if (res != 1 )
    { printf ("[f FooBar] = %d\n",  res); abort ();}
  
  /* Now check the short-cut CLASS.property syntax.  */
  /* Read... */
  res = f.FooBar;
  if (res != 1 )
    { printf ("f.FooBar = %d\n",  res); abort ();}
  
  /* .... write. */
  f.FooBar = 0;
  /* printf ("seems OK\n",  res); */
  return f.FooBar;
}

