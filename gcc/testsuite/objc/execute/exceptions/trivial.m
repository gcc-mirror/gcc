#include <stdlib.h>
#import "../../../objc-obj-c++-shared/Object1.h"

/* do nothing except prove we can compile and link code calling the
   ecceptions mechanism */
   
int main(void)
{
    @try {
	int a = 1 ;
	@throw [Object new];
    }
    @catch (Object *obj) {
  	return 0;
    }
    abort();
}

#import "../../../objc-obj-c++-shared/Object1-implementation.h"
