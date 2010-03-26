/* This provides a minimal implementation of the Object root class.
 * It is split from the definition so that it can be placed
 * at the end of source files that require it.  This reduces
 * clutter in .s and other internmediate code while debugging.
*/
#ifndef _OBJC_OBJECT1_IMPLEMENTATION_H_
#define _OBJC_OBJECT1_IMPLEMENTATION_H_
#ifdef DO_NEXT_M64_OBJECT_IMPLEMENTATION
@implementation Object

+ initialize {
     return self;
}
- init {
     return self;
}

+ class {
    return object_getClass(self);
}

+ new {
     return [[self alloc] init];
}

+ free {
     return nil;
}

- free {
     return object_dispose(self);
}

+ alloc {
     return class_createInstance(self, 0);
}


- class {
     return isa;
}


- superclass {
     return class_getSuperclass([self class]);
}

- (const char *) name {
     return class_getName([self class]);
}

-(BOOL)conformsTo:(Protocol *)protocol {
     Class cls;
     for (cls = [self class]; cls; cls = [cls superclass]) {
         if (class_conformsToProtocol(cls, protocol)) return YES;
     }
     return NO;
}

@end
#endif /* NEEDS_OBJECT_IMPLEMENTATION */
#endif /* _OBJC_OBJECT1_IMPLEMENTATION_H_ */