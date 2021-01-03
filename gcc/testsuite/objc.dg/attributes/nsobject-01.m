/* Test handling of the NSObject attribute.  */
/*  { dg-additional-options "-fsyntax-only " } */
/* { dg-additional-options "-Wno-objc-root-class" } */

typedef struct AnObj * __attribute__ ((NSObject)) AnObjRef;
typedef struct AnObj * __attribute__ ((__NSObject__)) AnotherObjRef;

/* We allow a void * to be labeled as NSObject.  */
typedef void *  __attribute__((NSObject)) AnonRef;

typedef struct AnObj * __attribute__((NSObject("foo"))) Bad; // { dg-error {wrong number of arguments specified for 'NSObject' attribute} }
typedef struct AnObj * __attribute__((NSObject(42))) Wrong; // { dg-error {wrong number of arguments specified for 'NSObject' attribute} }

/* Must be a pointer.  */
typedef struct AnObj  __attribute__((NSObject)) BadRef; // { dg-error {'NSObject' attribute is for pointer types only} }

typedef void * VPtr;

@interface CheckAttrNSObject
{
@public
  AnObjRef aor;
  /* TODO: synthesize without pre-defined ivars.  */
  VPtr obj_v;
  int bar;
  /* TODO: This should warn, even tho the property does not   */
   __attribute__((NSObject)) struct AnObj *Thing;
}

@property(copy) AnObjRef aor;

typedef struct AnObj * __attribute__((NSObject)) AnObjPtr3;
@property (nonatomic, retain) AnObjPtr3 obj_3;

@property (retain) __attribute__((NSObject)) VPtr obj_v;

//@property (strong, nullable) AnObjPtr3 objp_4;

@property(retain) __attribute__((NSObject)) int bar;
 // { dg-error {'NSObject' attribute is for pointer types only} "" { target *-*-* } .-1 }
 // { dg-error {'retain' attribute is only valid for Objective-C objects} "" { target *-*-* } .-2 }

@end

void foo ()
{
   __attribute__((NSObject)) struct AnObj *AnotherThing; // { dg-warning {'NSObject' attribute may be put on a typedef only; attribute is ignored} }
}

void
setProperty(id self, id value)
{
  ((CheckAttrNSObject *)self)->aor = value;
}

id 
getProperty(id self)
{
 return (id)((CheckAttrNSObject *)self)->aor;
}

@implementation CheckAttrNSObject
@synthesize aor;
@dynamic obj_3;
@synthesize obj_v;
@synthesize bar; // { dg-warning {returning 'id' from a function with return type 'int'} }
@end // { dg-warning {passing argument} }
