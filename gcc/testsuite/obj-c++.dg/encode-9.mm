/* Test than @encode is properly instantiated. */
/* { dg-do run } */

#include <string.h>           
#include <stdlib.h>
#include <objc/objc.h>

template<typename T>
class typeOf {
public:
    operator const char*() { return @encode(T); }
};

int main() {
    typeOf<int> t;
    if (strcmp ((const char *)t, @encode(int)))
      abort();

    typeOf<const char*> c;
    if (strcmp ((const char *)c, @encode(const char*)))
      abort();

    return 0;
}

