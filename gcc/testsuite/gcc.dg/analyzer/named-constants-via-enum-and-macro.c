#include "analyzer-decls.h"

enum __foo {
     O_ACCMODE = 1,
 
#define O_ACCMODE O_ACCMODE
};

void test_sm_fd_constants (void)
{
  __analyzer_dump_named_constant ("O_ACCMODE"); /* { dg-warning "named constant 'O_ACCMODE' has value '1'" } */
}
