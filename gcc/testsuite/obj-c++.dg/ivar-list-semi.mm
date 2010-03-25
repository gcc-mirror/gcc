/* Allow for an optional semicolon following the ivar block.  */
/* Contributed by: Ziemowit Laski <zlaski@apple.com>.  */

#include "../objc-obj-c++-shared/Object1.h"

@interface Tink : Object {
@private
 unsigned long mCode[4];
};
- (id)initWithProc:(void *)inProc;
- (void *)getUniqueProc;
@end
