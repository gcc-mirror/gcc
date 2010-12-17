/* Additional testing for the NeXT runtime.  */
/* Author: Ziemowit Laski  <zlaski@apple.com>  */

/* { dg-do run { target *-*-darwin* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-Wno-deprecated-declarations" } */
/* { dg-skip-if "" { *-*-* } { "-fgnu-runtime" } { "" } } */

#include "../objc-obj-c++-shared/Object1.h"
#include "../objc-obj-c++-shared/next-mapping.h"
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#define CHECK_IF(E) if (!(E)) abort ()

@class NSDictionary, NSFont, NSError, _NSATSTypesetterGuts, NSString, NSMenu, NSArray;

typedef unsigned char UInt8;
typedef const signed long OSStatus;
typedef unsigned long CFIndex;
typedef unsigned int UInt32;
typedef UInt32 FourCharCode;
typedef FourCharCode OSType;

struct FSRef {
  UInt8 hidden[80];
};
typedef struct FSRef FSRef;

typedef struct _NSPoint {
    float x;
    float y;
} NSPoint;

typedef struct _NSSize {
    float width;
    float height;
} NSSize;

typedef struct _NSRect {
    NSPoint origin;
    NSSize size;
} NSRect;

typedef struct _NSRange {
    unsigned int location;
    unsigned int length;
} NSRange;

typedef const char *NXAtom;

typedef struct {
  NSDictionary *_attributes;
  NSFont *_font;
  CFIndex _characterLength;
  CFIndex _nominalGlyphLocation;
  const CFIndex *p;
  float _defaultLineHeight;
  float _defaultBaselineOffset;
  float _horizExpansion;
  float _baselineDelta;
  NSRect _attachmentBBox;
  long ll, *llp;
  unsigned long ull, *ullp;
  id a;
  const id a1;
  const struct objc_object *a2;
  SEL b;
  const SEL b1;
  const struct objc_selector *b2;
  const char *str1;
  char *str2;
  char *const str3;
  const char *const str4;
  struct {
    unsigned int _isAttachmentRun:1;
    unsigned int _hasPositionalStake:1;
    unsigned int _isDefaultFace:1;
    unsigned int _hasCombiningMarks:1;
    unsigned int _isScreenFont:1;
    unsigned int _reserved:27;
  } _rFlags;
} NSATSGlyphStorageRun;

typedef struct __CFSet *CFMutableSetRef;
typedef const struct __CTLine * CTLineRef;
typedef const struct __NSAppleEventManagerSuspension* NSAppleEventManagerSuspensionID;

struct ComponentInstanceRecord {
  long data[1];
};
typedef struct ComponentInstanceRecord  ComponentInstanceRecord;
typedef ComponentInstanceRecord *ComponentInstance;

typedef NSString *(*NSErrorUserInfoFormatterFunc)(id objToBeDisplayed, NSError *err, char modifier);
typedef struct {
  NSErrorUserInfoFormatterFunc formatterFunc;
  NSString *userInfoKey;
  unsigned int parameterMask;
} NSErrorUserInfoFormatter;

typedef Object MyObj;
typedef Object *MyPtr;

@interface Foo: Object {
  NSATSGlyphStorageRun r;
}
- (NSError *)_errorWithOSStatus:(OSStatus)inOSStatus ref1:(const FSRef *)inRef1 ref2:(const struct FSRef *)inRef2
  reading:(BOOL)inReadingNotWriting;
- (const NSATSGlyphStorageRun *)_attributeRunForCharacterAtIndex:(const CFIndex)charIndex;
- (const _NSATSTypesetterGuts *)_getATSTypesetterGuts:(const struct objc_selector *)sel;
- (void)resumeWithSuspensionID:(NSAppleEventManagerSuspensionID)suspensionID and:(const CFIndex *)status;
- (const id)anotherMeth:(const SEL)sel and:(const Foo *)foo and:(const struct objc_object *)obj;
- (id)str1:(const char *)str1 str2:(char *)str2 str3:(char *const)str3 str4:(const char *const)str4;
- (oneway void)foo1:(Foo *)foo1 foo2:(const Foo *)foo2 foo3:(Foo *const)foo3 foo4:(const Foo *const)foo4;
- (in const char *)sel1:(const SEL)sel1 id1:(const id)id1;
- (inout id)obj1:(const MyPtr)obj1 obj2:(Object *const)obj2 obj3:(MyObj *const)obj3;
+ (ComponentInstance)_defaultScriptingComponent;
- (NSString *)_formatCocoaErrorString:(NSString *)formatString parameters:(const char *)parameters 
  applicableFormatters:(NSErrorUserInfoFormatter **)formatters count:(int)numFormatters;
- (NSErrorUserInfoFormatter *)formatter_func:(id)obj run:(const NSATSGlyphStorageRun **)run;
- (BOOL)_forgetWord:(bycopy in NSString *)word inDictionary:(bycopy in NSString *)language;
- (void)_registerServicesMenu:(NSMenu *)servicesMenu withSendTypes:(const NXAtom *)sendTypes 
  andReturnTypes:(const NXAtom *)returnTypes addToList:(BOOL)addToList;
+ (CFMutableSetRef *)_proxySharePointer;
- (NSRange)_checkGrammarInString:(in NSString *)stringToCheck language:(bycopy in NSString *)language details:(bycopy out NSArray **)details;
- (bool)_resolvePositionalStakeGlyphsForLineFragment:(CTLineRef)line lineFragmentRect:(NSRect)lineFragmentRect
  minPosition:(float)minPosition maxPosition:(float)maxPosition maxLineFragmentWidth:(float)maxLineFragmentWidth
  breakHint:(CFIndex *)charIndex;
+ (BOOL)findVoiceByIdentifier:(NSString *)identifier returningCreator:(OSType *)returnedCreator returningID:(OSType *)returnedID;
@end

NSRange globalRange;

@implementation Foo
- (NSError *)_errorWithOSStatus:(OSStatus)inOSStatus ref1:(const FSRef *)inRef1 ref2:(const struct FSRef *)inRef2
  reading:(BOOL)inReadingNotWriting {
  return (NSError *)self;
}
- (const NSATSGlyphStorageRun *)_attributeRunForCharacterAtIndex:(CFIndex)charIndex {
  return (const NSATSGlyphStorageRun *)self;
}
- (const _NSATSTypesetterGuts *)_getATSTypesetterGuts:(const struct objc_selector *)sel {
  return (const _NSATSTypesetterGuts *)self;
}
- (void)resumeWithSuspensionID:(NSAppleEventManagerSuspensionID)suspensionID and:(const CFIndex *)status {
}
- (const id)anotherMeth:(const SEL)sel and:(const Foo *)foo and:(const struct objc_object *)obj {
  return (const id)self;
}
- (id)str1:(const char *)str1 str2:(char *)str2 str3:(char *const)str3 str4:(const char *const)str4 {
  return self;
}
- (oneway void)foo1:(Foo *)foo1 foo2:(const Foo *)foo2 foo3:(Foo *const)foo3 foo4:(const Foo *const)foo4 {
}
- (in const char *)sel1:(const SEL)sel1 id1:(const id)id1 {
  return "Hello";
}  
- (inout id)obj1:(const MyPtr)obj1 obj2:(Object *const)obj2 obj3:(MyObj *const)obj3 {
  return self;
}
+ (ComponentInstance)_defaultScriptingComponent {
  return (ComponentInstance)0;
}
- (NSString *)_formatCocoaErrorString:(NSString *)formatString parameters:(const char *)parameters 
  applicableFormatters:(NSErrorUserInfoFormatter **)formatters count:(int)numFormatters {
  return (NSString *)self;
}
- (NSErrorUserInfoFormatter *)formatter_func:(id)obj run:(const NSATSGlyphStorageRun **)run {
  return (NSErrorUserInfoFormatter *)0;
}
- (BOOL)_forgetWord:(bycopy in NSString *)word inDictionary:(bycopy in NSString *)language {
  return YES;
}
- (void)_registerServicesMenu:(NSMenu *)servicesMenu withSendTypes:(const NXAtom *)sendTypes 
  andReturnTypes:(const NXAtom *)returnTypes addToList:(BOOL)addToList {
}
+ (CFMutableSetRef *)_proxySharePointer {
  return (CFMutableSetRef *)0;
}
- (NSRange)_checkGrammarInString:(in NSString *)stringToCheck language:(bycopy in NSString *)language details:(bycopy out NSArray **)details {
  return globalRange;
}
- (bool)_resolvePositionalStakeGlyphsForLineFragment:(CTLineRef)line lineFragmentRect:(NSRect)lineFragmentRect 
  minPosition:(float)minPosition maxPosition:(float)maxPosition maxLineFragmentWidth:(float)maxLineFragmentWidth 
  breakHint:(CFIndex *)charIndex {
  return false;
}
+ (BOOL)findVoiceByIdentifier:(NSString *)identifier returningCreator:(OSType *)returnedCreator returningID:(OSType *)returnedID {
  return NO;
}
@end

int main(void) {
  Class fooClass = objc_getClass ("Foo");
  Method meth;
  struct objc_ivar_list *ivars;
  struct objc_ivar *ivar;

  meth = class_getInstanceMethod (fooClass, @selector(_errorWithOSStatus:ref1:ref2:reading:));
  CHECK_IF (!strcmp (meth->method_types, "@24@0:4l8r^{FSRef=[80C]}12r^{FSRef=[80C]}16c20"));

  meth = class_getInstanceMethod (fooClass, @selector(_attributeRunForCharacterAtIndex:));
  CHECK_IF (!strcmp (meth->method_types, "r^{?=@@II^Iffff{_NSRect={_NSPoint=ff}{_NSSize=ff}}l^lL^L@@@:::****{?=b1b1b1b1b1b27}}12@0:4L8"));

  meth = class_getInstanceMethod (fooClass, @selector(_getATSTypesetterGuts:));
  CHECK_IF (!strcmp (meth->method_types, "r@12@0:4r:8"));

  meth = class_getInstanceMethod (fooClass, @selector(resumeWithSuspensionID:and:));
  CHECK_IF (!strcmp (meth->method_types, "v16@0:4^{__NSAppleEventManagerSuspension=}8r^I12"));

  meth = class_getInstanceMethod (fooClass, @selector(anotherMeth:and:and:));
  CHECK_IF (!strcmp (meth->method_types, "r@20@0:4r:8r@12r@16"));

  meth = class_getInstanceMethod (fooClass, @selector(str1:str2:str3:str4:));
  CHECK_IF (!strcmp (meth->method_types, "@24@0:4r*8*12*16r*20"));

  meth = class_getInstanceMethod (fooClass, @selector(foo1:foo2:foo3:foo4:));
  CHECK_IF (!strcmp (meth->method_types, "Vv24@0:4@8r@12@16r@20"));

  meth = class_getInstanceMethod (fooClass, @selector(sel1:id1:));
  CHECK_IF (!strcmp (meth->method_types, "rn*16@0:4r:8r@12"));

  meth = class_getInstanceMethod (fooClass, @selector(obj1:obj2:obj3:));
  CHECK_IF (!strcmp (meth->method_types, "N@20@0:4r@8@12^{Object=#}16"));

  meth = class_getClassMethod (fooClass, @selector(_defaultScriptingComponent));
  CHECK_IF (!strcmp (meth->method_types, "^{ComponentInstanceRecord=[1l]}8@0:4"));

  meth = class_getInstanceMethod (fooClass, @selector(_formatCocoaErrorString:parameters:applicableFormatters:count:));
  CHECK_IF (!strcmp (meth->method_types, "@24@0:4@8r*12^^{?}16i20"));

  meth = class_getInstanceMethod (fooClass, @selector(formatter_func:run:));
  CHECK_IF (!strcmp (meth->method_types, "^{?=^?@I}16@0:4@8r^^{?}12"));

  meth = class_getInstanceMethod (fooClass, @selector(_forgetWord:inDictionary:));
  CHECK_IF (!strcmp (meth->method_types, "c16@0:4nO@8nO@12"));

  meth = class_getInstanceMethod (fooClass, @selector(_registerServicesMenu:withSendTypes:andReturnTypes:addToList:));
  CHECK_IF (!strcmp (meth->method_types, "v24@0:4@8r^*12r^*16c20"));

  meth = class_getClassMethod (fooClass, @selector(_proxySharePointer));
  CHECK_IF (!strcmp (meth->method_types, "^^{__CFSet}8@0:4"));

  meth = class_getInstanceMethod (fooClass, @selector(_checkGrammarInString:language:details:));
  CHECK_IF (!strcmp (meth->method_types, "{_NSRange=II}20@0:4n@8nO@12oO^@16"));

  meth = class_getInstanceMethod (fooClass, @selector(_resolvePositionalStakeGlyphsForLineFragment:lineFragmentRect:minPosition:maxPosition:maxLineFragmentWidth:breakHint:));
  CHECK_IF (!strcmp (meth->method_types, "B44@0:4^{__CTLine=}8{_NSRect={_NSPoint=ff}{_NSSize=ff}}12f28f32f36^I40"));

  meth = class_getClassMethod (fooClass, @selector(findVoiceByIdentifier:returningCreator:returningID:));
  CHECK_IF (!strcmp (meth->method_types, "c20@0:4@8^I12^I16"));

  ivars = fooClass->ivars;
  CHECK_IF (ivars->ivar_count == 1);

  ivar = ivars->ivar_list;
  CHECK_IF (!strcmp (ivar->ivar_name, "r"));
  CHECK_IF (!strcmp (ivar->ivar_type,
    "{?=\"_attributes\"@\"NSDictionary\"\"_font\"@\"NSFont\"\"_characterLength\""
    "I\"_nominalGlyphLocation\"I\"p\"^I\"_defaultLineHeight\"f\"_defaultBaselineOffset\""
    "f\"_horizExpansion\"f\"_baselineDelta\"f\"_attachmentBBox\"{_NSRect=\"origin\""
    "{_NSPoint=\"x\"f\"y\"f}\"size\"{_NSSize=\"width\"f\"height\"f}}\"ll\"l\"llp\"^l\"ull\""
    "L\"ullp\"^L\"a\"@\"a1\"@\"a2\"@\"b\":\"b1\":\"b2\":\"str1\"*\"str2\"*\"str3\"*\"str4\""
    "*\"_rFlags\"{?=\"_isAttachmentRun\"b1\"_hasPositionalStake\"b1\"_isDefaultFace\""
    "b1\"_hasCombiningMarks\"b1\"_isScreenFont\"b1\"_reserved\"b27}}"));

  return 0;
}
