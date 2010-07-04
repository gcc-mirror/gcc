#import "Object1.h"
/* This will generate the code if required - as determined by
   the headr above.  It is kept like this to keep one code file
   shared between dg-xxxx tests that can ask for an extra source
   and the objc/{compile,execute}/xxx tests that have to include
   the implementation explicitly.
   
   For cases/targets that don't require the generation of the
   Object implementation, this should result in an empty object.
*/
#import "Object1-implementation.h"
