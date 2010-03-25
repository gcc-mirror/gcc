/*
 * Contributed by Nicola Pero <nicola@brainstorm.co.uk>
 * Fri Feb  2 11:48:01 GMT 2001
 */

#include "../../objc-obj-c++-shared/Protocol1.h"

@protocol MyProtocol
- (bycopy id) bycopyMethod;
@end

int main (void)
{
  [nil bycopyMethod];

   exit (0);
}


