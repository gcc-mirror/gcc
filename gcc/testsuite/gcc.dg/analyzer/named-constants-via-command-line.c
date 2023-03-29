/* { dg-additional-options "-DO_ACCMODE=42 -DO_RDONLY=0x1 -DO_WRONLY=010" } */

#include "analyzer-decls.h"

void test_sm_fd_constants (void)
{
  __analyzer_dump_named_constant ("O_ACCMODE"); /* { dg-warning "named constant 'O_ACCMODE' has value '42'" } */
  __analyzer_dump_named_constant ("O_RDONLY"); /* { dg-warning "named constant 'O_RDONLY' has value '1'" } */
  __analyzer_dump_named_constant ("O_WRONLY"); /* { dg-warning "named constant 'O_WRONLY' has value '8'" } */
}
