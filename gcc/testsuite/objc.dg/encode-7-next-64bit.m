/* Additional testing for the NeXT runtime. Encoding in -m64 mode  */

/* { dg-do run { target *-*-darwin* } } */
/* { dg-require-effective-target lp64 } */
/* { dg-skip-if "" { *-*-* } { "-fgnu-runtime" } { "" } } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <objc/Object.h>
#include "../objc-obj-c++-shared/runtime.h"

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
  Ivar *ivars;
  unsigned int ivar_count;
  Ivar ivar;

  meth = class_getInstanceMethod (fooClass, @selector(_errorWithOSStatus:ref1:ref2:reading:));
  CHECK_IF (!strcmp (method_getTypeEncoding(meth), "@44@0:8q16r^{FSRef=[80C]}24r^{FSRef=[80C]}32c40"));

  meth = class_getInstanceMethod (fooClass, @selector(_attributeRunForCharacterAtIndex:));
  CHECK_IF (!strcmp (method_getTypeEncoding (meth), "r^{?=@@QQ^Qffff{_NSRect={_NSPoint=ff}{_NSSize=ff}}q^qQ^Q@@@:::****{?=b1b1b1b1b1b27}}24@0:8Q16"));

  meth = class_getInstanceMethod (fooClass, @selector(_getATSTypesetterGuts:));
  CHECK_IF (!strcmp (method_getTypeEncoding (meth), "r@24@0:8r:16"));

  meth = class_getInstanceMethod (fooClass, @selector(resumeWithSuspensionID:and:));
  CHECK_IF (!strcmp (method_getTypeEncoding (meth), "v32@0:8^{__NSAppleEventManagerSuspension=}16r^Q24"));

  meth = class_getInstanceMethod (fooClass, @selector(anotherMeth:and:and:));
  CHECK_IF (!strcmp (method_getTypeEncoding (meth), "r@40@0:8r:16r@24r@32"));

  meth = class_getInstanceMethod (fooClass, @selector(str1:str2:str3:str4:));
  CHECK_IF (!strcmp (method_getTypeEncoding (meth), "@48@0:8r*16*24*32r*40"));

  meth = class_getInstanceMethod (fooClass, @selector(foo1:foo2:foo3:foo4:));
  CHECK_IF (!strcmp (method_getTypeEncoding (meth), "Vv48@0:8@16r@24@32r@40"));

  meth = class_getInstanceMethod (fooClass, @selector(sel1:id1:));
  CHECK_IF (!strcmp (method_getTypeEncoding (meth), "rn*32@0:8r:16r@24"));

  meth = class_getInstanceMethod (fooClass, @selector(obj1:obj2:obj3:));
  CHECK_IF (!strcmp (method_getTypeEncoding (meth), "N@40@0:8r@16@24^{Object=#}32"));

  meth = class_getClassMethod (fooClass, @selector(_defaultScriptingComponent));
  CHECK_IF (!strcmp (method_getTypeEncoding (meth), "^{ComponentInstanceRecord=[1q]}16@0:8"));

  meth = class_getInstanceMethod (fooClass, @selector(_formatCocoaErrorString:parameters:applicableFormatters:count:));
  CHECK_IF (!strcmp (method_getTypeEncoding (meth), "@44@0:8@16r*24^^{?}32i40"));

  meth = class_getInstanceMethod (fooClass, @selector(formatter_func:run:));
  CHECK_IF (!strcmp (method_getTypeEncoding (meth), "^{?=^?@I}32@0:8@16r^^{?}24"));

  meth = class_getInstanceMethod (fooClass, @selector(_forgetWord:inDictionary:));
  CHECK_IF (!strcmp (method_getTypeEncoding (meth), "c32@0:8nO@16nO@24"));

  meth = class_getInstanceMethod (fooClass, @selector(_registerServicesMenu:withSendTypes:andReturnTypes:addToList:));
  CHECK_IF (!strcmp (method_getTypeEncoding (meth), "v44@0:8@16r^*24r^*32c40"));

  meth = class_getClassMethod (fooClass, @selector(_proxySharePointer));
  CHECK_IF (!strcmp (method_getTypeEncoding (meth), "^^{__CFSet}16@0:8"));

  meth = class_getInstanceMethod (fooClass, @selector(_checkGrammarInString:language:details:));
  CHECK_IF (!strcmp (method_getTypeEncoding (meth), "{_NSRange=II}40@0:8n@16nO@24oO^@32"));

  meth = class_getInstanceMethod (fooClass, @selector(_resolvePositionalStakeGlyphsForLineFragment:lineFragmentRect:minPosition:maxPosition:maxLineFragmentWidth:breakHint:));
  CHECK_IF (!strcmp (method_getTypeEncoding (meth), "B60@0:8^{__CTLine=}16{_NSRect={_NSPoint=ff}{_NSSize=ff}}24f40f44f48^Q52"));

  meth = class_getClassMethod (fooClass, @selector(findVoiceByIdentifier:returningCreator:returningID:));
  CHECK_IF (!strcmp (method_getTypeEncoding (meth), "c40@0:8@16^I24^I32"));

  ivars = class_copyIvarList (fooClass, &ivar_count);
  CHECK_IF (ivar_count == 1);

  ivar = ivars[0];
  CHECK_IF (!strcmp (ivar_getName(ivar), "r"));
  CHECK_IF (!strcmp (ivar_getTypeEncoding(ivar),
   "{?=\"_attributes\"@\"NSDictionary\"\"_font\"@\"NSFont\"\"_characterLength\""
    "Q\"_nominalGlyphLocation\"Q\"p\"^Q\"_defaultLineHeight\"f\"_defaultBaselineOffset\""
    "f\"_horizExpansion\"f\"_baselineDelta\"f\"_attachmentBBox\"{_NSRect=\"origin\""
    "{_NSPoint=\"x\"f\"y\"f}\"size\"{_NSSize=\"width\"f\"height\"f}}\"ll\"q\"llp\"^q\"ull\""
    "Q\"ullp\"^Q\"a\"@\"a1\"@\"a2\"@\"b\":\"b1\":\"b2\":\"str1\"*\"str2\"*\"str3\"*\"str4\""
    "*\"_rFlags\"{?=\"_isAttachmentRun\"b1\"_hasPositionalStake\"b1\"_isDefaultFace\""
    "b1\"_hasCombiningMarks\"b1\"_isScreenFont\"b1\"_reserved\"b27}}"));

  return 0;
}
