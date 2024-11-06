/* Regression test for interaction of named constants in -fanalyzer with
   -Wunused-macros (PR analyzer/107711).  */

/* { dg-additional-options "-Wunused-macros" } */

#include "analyzer-decls.h"

/* Various constants used by the fd state machine.  */

#define O_ACCMODE 42   /* { dg-warning "-: macro 'O_ACCMODE' is not used" } */
#define O_RDONLY  0x1  /* { dg-warning "-: macro 'O_RDONLY' is not used" } */
#define O_WRONLY  010  /* { dg-warning "-: macro 'O_WRONLY' is not used" } */

void test_sm_fd_constants (void)
{
  __analyzer_dump_named_constant ("O_ACCMODE"); /* { dg-warning "named constant 'O_ACCMODE' has value '42'" } */
  __analyzer_dump_named_constant ("O_RDONLY"); /* { dg-warning "named constant 'O_RDONLY' has value '1'" } */
  __analyzer_dump_named_constant ("O_WRONLY"); /* { dg-warning "named constant 'O_WRONLY' has value '8'" } */
}
