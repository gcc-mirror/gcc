/* The implementation of class Object for Objective-C.
   Copyright (C) 1993 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

GNU CC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, if you link this library with files compiled
   with GCC to produce an executable, this does not cause the resulting
   executable to be covered by the GNU General Public License.  This
   exception does not however invalidate any other reasons why the
   executable file might be covered by the GNU General Public License. */

#include <objc/Object.h>
#include <objc/Protocol.h>
#include <objc/objc-api.h>
#include <objc/objc-archive.h>

extern int strlen(const char *);
extern int strcmp(const char *, const char *);
extern int read(int, void *, int);
extern int write(int, const void *, int);
extern int errno;

#define MAX_CLASS_NAME_LEN 256

@implementation Object

// Initialize a class
// often overridden by subclasses
+ initialize
{
  return self;
}

// Initialize an instance
// this method does not apply to class objects
// often overridden by subclasses (should call superclass version)
- init
{
  return self;
}

// Create and initialize an instance of a class
// not usually overridden by subclasses (should call superclass version)
+ new
{
  return [[self alloc] init];
}

// Creates an instance of a class
// should NOT be overridden by subclasses
+ alloc
{
  return class_create_instance(self);
}

// Free an instance
// this method does not apply to class objects
// often overridden by subclasses (should call superclass version)
- free
{
  return object_dispose(self);
}

// Create a copy of the receiving instance
// this method does not apply to class objects
// not usually overridden by subclasses
- copy
{
  return [[self shallowCopy] deepen];
}

// Creates a copy of only the receiving instance
// this method does not apply to class objects
// should NOT be overridden by subclasses
- shallowCopy
{
  return object_copy(self);
}

// Deepens a shallow copy of an instance
// this method does not apply to class objects
// often overridden by subclasses (should call superclass version)
- deepen
{
  return self;
}

// Creates a recursive copy of the receiving instance
// this method does not apply to class objects
// may be overridden by subclasses
// Not correct, but included for compatibility with Stepstone
- deepCopy
{
  return [self copy];
}

// Return the class object or the class of an instance
// not usually overridden by subclasses
- (Class_t)class
{
  return object_get_class(self);
}

// Return the superclass of a class or instance
// not usually overridden by subclasses
- (Class_t)superClass
{
  return object_get_super_class(self);
}

// Return the metaclass of a class or instance
// not usually overridden by subclasses
- (MetaClass_t)metaClass
{
  return object_get_meta_class(self);
}

// Return the character string name of a class or an instance's class
// not usually overridden by subclasses
- (const char *)name
{
  return object_get_class_name(self);
}

// Return the receiving class or instance object
// not usually overridden by subclasses
- self
{
  return self;
}

// Return a hash value for a class or instance object
// not usually overridden by subclasses
- (unsigned int)hash
{
  return (unsigned int)self;
}

// Indicates if anObject is the receiving class or instance object
// not usually overridden by subclasses
- (BOOL)isEqual:anObject
{
  return self==anObject;
}

// Indicates if the receiver is a metaclass object
// should NOT be overridden by subclasses
- (BOOL)isMetaClass
{
  return NO;
}

// Indicates if the receiver is a class object
// should NOT be overridden by subclasses
- (BOOL)isClass
{
  return object_is_class(self);
}

// Indicates if the receiver is an instance object
// should NOT be overridden by subclasses
- (BOOL)isInstance
{
  return object_is_instance(self);
}

// Indicates if the receiver is a type of aClassObject
// not usually overridden by subclasses
- (BOOL)isKindOf:(Class_t)aClassObject
{
  Class_t class;

  for (class = self->isa; class!=Nil; class = class_get_super_class(class))
    if (class==aClassObject)
      return YES;
  return NO;
}

// Indicates if the receiver is a member of the aClassObject class
// not usually overridden by subclasses
- (BOOL)isMemberOf:(Class_t)aClassObject
{
  return self->isa==aClassObject;
}

// Indicates if the receiver is a type of the class named aClassName
// not usually overridden by subclasses
- (BOOL)isKindOfClassNamed:(const char *)aClassName
{
  Class_t class;

  if (aClassName!=NULL)
    for (class = self->isa; class!=Nil; class = class_get_super_class(class))
      if (!strcmp(class_get_class_name(class), aClassName))
        return YES;
  return NO;
}

// Indicates if the receiver is a member of the class named aClassName
// not usually overridden by subclasses
- (BOOL)isMemberOfClassNamed:(const char *)aClassName
{
  return ((aClassName!=NULL)
          &&!strcmp(class_get_class_name(self->isa), aClassName));
}

// Indicates if instances of a class respond to the message aSel
// not usually overridden by subclasses
+ (BOOL)instancesRespondTo:(SEL)aSel
{
  return class_get_instance_method(self, aSel)!=METHOD_NULL;
}

// Indicates if the receiving class or instance responds to the message aSel
// not usually overridden by subclasses
- (BOOL)respondsTo:(SEL)aSel
{
  return ((object_is_instance(self)
           ?class_get_instance_method(self->isa, aSel)
           :class_get_class_method(self->isa, aSel))!=METHOD_NULL);
}

// Returns the address of a class's instance method
// not usually overridden by subclasses
+ (IMP)instanceMethodFor:(SEL)aSel
{
  return method_get_imp(class_get_instance_method(self, aSel));
}

// Indicates if the receiving class or instance conforms to the given protocol
// not usually overridden by subclasses
- (BOOL) conformsTo: (Protocol*)aProtocol
{
  int i;
  struct objc_protocol_list* proto_list;

  for (proto_list = isa->protocols;
       proto_list; proto_list = proto_list->next)
    {
      for (i=0; i < proto_list->count; i++)
      {
        if ([proto_list->list[i] conformsTo: aProtocol])
          return YES;
      }
    }

  return NO;
}


// Returns the address of a class's class or an instance's instance method
// not usually overridden by subclasses
- (IMP)methodFor:(SEL)aSel
{
  return (method_get_imp(object_is_instance(self)
                         ?class_get_instance_method(self->isa, aSel)
                         :class_get_class_method(self->isa, aSel)));
}

// Returns a method description for a class's instance method aSel
// not usually overridden by subclasses
+ (struct objc_method_description *)descriptionForInstanceMethod:(SEL)aSel
{
  return ((struct objc_method_description *)
           class_get_instance_method(self, aSel));
}

// Returns a description for a class's class or an instance's instance method
// not usually overridden by subclasses
- (struct objc_method_description *)descriptionForMethod:(SEL)aSel
{
  return ((struct objc_method_description *)
           (object_is_instance(self)
            ?class_get_instance_method(self->isa, aSel)
            :class_get_class_method(self->isa, aSel)));
}

// Sends the message aSel, which takes no parameters, to the receiver
// not usually overridden by subclasses
- perform:(SEL)aSel
{
  IMP msg = objc_msg_lookup(self, aSel);
  if (!msg)
    return [self error:"invalid selector passed to %s", sel_get_name(_cmd)];
  return (*msg)(self, aSel);
}

// Sends the message aSel, which takes one id parameter, to the receiver
// not usually overridden by subclasses
- perform:(SEL)aSel with:anObject
{
  IMP msg = objc_msg_lookup(self, aSel);
  if (!msg)
    return [self error:"invalid selector passed to %s", sel_get_name(_cmd)];
  return (*msg)(self, aSel, anObject);
}

// Sends the message aSel, which takes two id parameters, to the receiver
// not usually overridden by subclasses
- perform:(SEL)aSel with:anObject1 with:anObject2
{
  IMP msg = objc_msg_lookup(self, aSel);
  if (!msg)
    return [self error:"invalid selector passed to %s", sel_get_name(_cmd)];
  return (*msg)(self, aSel, anObject1, anObject2);
}

// Forwards a message to which a class or instance object does not respond
// may be overridden by subclasses
- forward:(SEL)aSel :(arglist_t)argFrame
{
  return [self doesNotRecognize: aSel];
}

// Sends a message aSel, of arbitrary arguments, to the receiver
// should NOT be overridden by subclasses
- performv:(SEL)aSel :(arglist_t)argFrame
{
  return objc_msg_sendv(self, aSel, method_get_argsize(0), argFrame);
}

// Instructs the runtime system that the receiver is to pose for aClassObject
// not usually overridden by subclasses
+ poseAs:(Class_t)aClassObject
{
  return class_pose_as(self, aClassObject);
}

// Changes the receiver's class to be aClassObject
// this method does not apply to class objects
// not usually overridden by subclasses
- (Class_t)transmuteClassTo:(Class_t)aClassObject
{
  if (object_is_instance(self))
    if (class_is_class(aClassObject))
      if (class_get_instance_size(aClassObject)==class_get_instance_size(isa))
        if ([self isKindOf:aClassObject])
          {
            Class_t old_isa = isa;
            isa = aClassObject;
            return old_isa;
          }
  return nil;
}

// Indicates that a subclass did not override a class or instance message
//   it was supposed to have overridden
// not usually overridden by subclasses
- subclassResponsibility:(SEL)aSel
{
  return [self error:"subclass should override %s", sel_get_name(aSel)];
}

// Indicates that a class or instance method has not been implemented
// may be overridden by subclasses
- notImplemented:(SEL)aSel
{
  return [self error:"method %s not implemented", sel_get_name(aSel)];
}

// Reports that a class or instance does not recognize the message aSel
// not usually overridden by subclasses
- doesNotRecognize:(SEL)aSel
{
  return [self error:"%s does not recognize %s",
                     object_get_class_name(self), sel_get_name(aSel)];
}

// Reports an error
// not usually overridden by subclasses
- error:(const char *)aString, ...
{
#define FMT "error: %s (%s)\n%s\n"
  char fmt[(strlen(FMT)+strlen(object_get_class_name(self))
            +((aString!=NULL)?strlen(aString):0)+8)];
  va_list ap;

  sprintf(fmt, FMT, object_get_class_name(self),
                    object_is_instance(self)?"instance":"class",
                    (aString!=NULL)?aString:"");
  va_start(ap, aString);
  (*_objc_error)(self, fmt, ap);
  va_end(ap);
  return nil;
#undef FMT
}

// Returns the class's version number
// not usually overridden by subclasses
+ (int)version
{
  return class_get_version(self);
}

// Sets the class's version number
// not usually overridden by subclasses
+ setVersion:(int)aVersion
{
  class_set_version(self, aVersion);
  return self;
}

// These are used to write or read the instance variables 
// declared in this particular part of the object.  Subclasses
// should extend these, by calling [super read/write: aStream]
// before doing their own archiving.  These methods are private, in
// the sense that they should only be called from subclasses.

- read: (TypedStream*)aStream
{
  // [super read: aStream];  
  return self;
}

- write: (TypedStream*)aStream
{
  // [super write: aStream];
  return self;
}

// These are used to read or write class information, such as static
// variables used in that class.  The version number of the class being 
// read can be obtained from 
//     objc_typed_stream_class_version(stream, class_name)  

+ write: (TypedStream*)aStream
{
  // [super write: aStream];
  return self;
}

+ read: (TypedStream*)aStream
{
  // [super read: aStream];
  return self;
}



@end
