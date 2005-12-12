#include "struct-layout-1.h"

#define TX(n, type, attrs, fields, ops) 			\
type S##n { fields } attrs;					\
void test##n (void)						\
{								\
  if (objc_sizeof_type (@encoding (type S##n)) != sizeof(type S##n)) \
    fails ++;							\
}
