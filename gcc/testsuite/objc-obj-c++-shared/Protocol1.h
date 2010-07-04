/* 
 * Temporary work-around to avoid the need for method attributes in
 * the NeXT Runtime Protocol header.
 */
#ifndef _OBJC_PROTOCOL1_H_
#define _OBJC_PROTOCOL1_H_

#  ifndef __NEXT_RUNTIME__
#    include <objc/Protocol.h>
#  else
#    include "next-abi.h"
#    ifndef NEXT_OBJC_USE_NEW_INTERFACE
/* We are on a NeXT version without method __attributes__ */
#      import <objc/Protocol.h>
#    else
/* We make our own interface without the deprecation messages 
 * This is essentially <objc/Protocol.h> without the OBJC2
 * flags.
 * 
 */
#      ifndef _OBJC_PROTOCOL_H_
#      define _OBJC_PROTOCOL_H_
#      import "Object1.h"

@interface Protocol : Object
{
@private
    char *protocol_name ;
    struct objc_protocol_list *protocol_list ;
    struct objc_method_description_list *instance_methods ;
    struct objc_method_description_list *class_methods ;
}

/* Obtaining attributes intrinsic to the protocol */
#if (NEXT_OBJC_ABI_VERSION==0)
- (const char *)name ; /* Not avail in v2, deprecated in prior */
/* Testing protocol conformance */
- (BOOL) conformsTo: (Protocol *)aProtocolObject ; /* Not avail in v2 */
#endif

/* Looking up information specific to a protocol */
/* Deprecated, but available */

- (struct objc_method_description *) descriptionForInstanceMethod:(SEL)aSel ;
- (struct objc_method_description *) descriptionForClassMethod:(SEL)aSel ;

@end

#      endif /* __NEXT_RUNTIME__ */
#    endif /* _OBJC_PROTOCOL_H_ */
#  endif /* NEXT_OBJC_ABI_VERSION */
#endif /* _OBJC_PROTOCOL1_H_ */
