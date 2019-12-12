/* Check that typedefs of ObjC classes preserve 
   any @protocol qualifiers.  */
/* { dg-do compile } */

#ifdef __NEXT_RUNTIME__
#include "../objc-obj-c++-shared/F-NSObject.h"
#define OBJECT NSObject
#else
#include <objc/Object.h>
#define OBJECT Object
#endif

@protocol CanDoStuff;

typedef OBJECT<CanDoStuff> CanDoStuffType;
typedef OBJECT<CanDoStuff> *CanDoStuffTypePtr;

@protocol CanDoStuff
- (int) dostuff;
@end

@protocol MoreStuff
- (int) morestuff;
@end

int main(void)
{
    CanDoStuffTypePtr  dice     = nil;
    CanDoStuffType    *nodice   = nil;
    int count;
    count = [dice dostuff];
    count = [nodice dostuff];
    return 0;
}

