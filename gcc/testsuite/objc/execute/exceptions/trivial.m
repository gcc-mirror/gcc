#include <stdlib.h>
#include "../../../objc-obj-c++-shared/TestsuiteObject.m"

/* do nothing except prove we can compile and link code calling the
   ecceptions mechanism */
   
int main(void)
{
    @try {
	int a = 1 ;
	@throw [TestsuiteObject new];
    }
    @catch (TestsuiteObject *obj) {
  	return 0;
    }
    abort();
}
