/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, March 2011.  */
/* Test encoding properties.  */
/* { dg-do run } */
/* { dg-skip-if "No API#2 pre-Darwin9" { *-*-darwin[5-8]* } { "-fnext-runtime" } { "" } } */

#include <objc/runtime.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

@interface MyRootClass
{ Class isa; }
+ alloc;
- init;
+ initialize;
@end

@implementation MyRootClass
+ alloc { return class_createInstance (self, 0); }
- init  { return self; }
+ initialize { return self; }
@end

@interface MySubClass : MyRootClass
{
  char char_property;
  short short_property;
  int int_property;
  long long_property;
  float float_property;
  double double_property;
  int *int_pointer_property;

  id propertyA;
  id propertyB;
  id propertyC;
  id propertyD;
  int propertyE;
  id propertyF;

  id other_variable;
}
@property char char_property;
@property short short_property;
@property int int_property;
@property long long_property;
@property float float_property;
@property double double_property;
@property int *int_pointer_property;

@property (assign, getter=getP, setter=setP:) id propertyA;
@property (assign) id propertyB;
@property (copy) id propertyC;
@property (retain) id propertyD;
@property (nonatomic) int propertyE;
@property (nonatomic, readonly, copy) id propertyF;

@property (assign) id propertyG;
@property (assign, readonly, getter=X) id propertyH;
@end

@implementation MySubClass
@synthesize char_property;
@synthesize short_property;
@synthesize int_property;
@synthesize long_property;
@synthesize float_property;
@synthesize double_property;
@synthesize int_pointer_property;

@synthesize propertyA;
@synthesize propertyB;
@synthesize propertyC;
@synthesize propertyD;
@synthesize propertyE;
@synthesize propertyF;

@synthesize propertyG = other_variable;
@dynamic propertyH;
@end

#ifdef __OBJC2__
void error (objc_property_t p)
{
  printf ("Error - property_getAttributes (\"%s\") returns \"%s\"\n",
	  property_getName (p),
	  property_getAttributes (p));
  abort ();
}

/* Concatenate 3 strings and return the result.  */
char *concat (const char *a, const char *b, const char *c)
{
  /* We happily leak memory here.  This is a test.  */
  char *x = (char *)malloc (sizeof (char) * 128);
  snprintf (x, 128, "%s%s%s", a, b, c);
  return x;
}

#endif

int main (void)
{
#ifdef __OBJC2__
  Class c = objc_getClass ("MySubClass");
  objc_property_t p;

  p = class_getProperty (c, "char_property");
  /* Usually we expect "Tc,Vchar_property", but if a char is of
     different size, it may be encoded differently than "c".  */
  if (strcmp (concat ("T", @encode (char), ",Vchar_property"),
	      property_getAttributes (p)) != 0)
    error (p);

  p = class_getProperty (c, "short_property");
  if (strcmp (concat ("T", @encode (short), ",Vshort_property"),
	      property_getAttributes (p)) != 0)
    error (p);

  p = class_getProperty (c, "int_property");
  if (strcmp (concat ("T", @encode (int), ",Vint_property"),
	      property_getAttributes (p)) != 0)
    error (p);

  p = class_getProperty (c, "long_property");
  if (strcmp (concat ("T", @encode (long), ",Vlong_property"),
	      property_getAttributes (p)) != 0)
    error (p);

  p = class_getProperty (c, "float_property");
  if (strcmp (concat ("T", @encode (float), ",Vfloat_property"),
	      property_getAttributes (p)) != 0)
    error (p);

  p = class_getProperty (c, "double_property");
  if (strcmp (concat ("T", @encode (double), ",Vdouble_property"),
	      property_getAttributes (p)) != 0)
    error (p);

  p = class_getProperty (c, "int_pointer_property");
  if (strcmp (concat ("T", @encode (int *), ",Vint_pointer_property"),
	      property_getAttributes (p)) != 0)
    error (p);

  /* Objects are always encoded as '@' hence the string does not
     depend on the architecture.  */
  p = class_getProperty (c, "propertyA");
  if (strcmp ("T@,GgetP,SsetP:,VpropertyA", property_getAttributes (p)) != 0)
    error (p);

  p = class_getProperty (c, "propertyB");
  if (strcmp ("T@,VpropertyB", property_getAttributes (p)) != 0)
    error (p);

  p = class_getProperty (c, "propertyC");
  if (strcmp ("T@,C,VpropertyC", property_getAttributes (p)) != 0)
    error (p);

  p = class_getProperty (c, "propertyD");
  if (strcmp ("T@,&,VpropertyD", property_getAttributes (p)) != 0)
    error (p);

  p = class_getProperty (c, "propertyE");
  if (strcmp (concat ("T", @encode (int), ",N,VpropertyE"),
	      property_getAttributes (p)) != 0)
    error (p);

  p = class_getProperty (c, "propertyF");
  if (strcmp ("T@,R,C,N,VpropertyF", property_getAttributes (p)) != 0)
    error (p);

  p = class_getProperty (c, "propertyG");
  if (strcmp ("T@,Vother_variable", property_getAttributes (p)) != 0)
    error (p);

  p = class_getProperty (c, "propertyH");
  if (strcmp ("T@,R,D,GX", property_getAttributes (p)) != 0)
    error (p);
#endif

  return 0;
}
