#include "analyzer-decls.h"

/* Various constants used by the fd state machine.  */

#define O_ACCMODE 42
#define O_RDONLY  0x1
#define O_WRONLY  010

void test_sm_fd_constants (void)
{
  __analyzer_dump_named_constant ("O_ACCMODE"); /* { dg-warning "named constant 'O_ACCMODE' has value '42'" } */
  __analyzer_dump_named_constant ("O_RDONLY"); /* { dg-warning "named constant 'O_RDONLY' has value '1'" } */
  __analyzer_dump_named_constant ("O_WRONLY"); /* { dg-warning "named constant 'O_WRONLY' has value '8'" } */
}

void test_unknown (void)
{
  __analyzer_dump_named_constant ("UNKNOWN"); /* { dg-warning "named constant 'UNKNOWN' has unknown value" } */
}
