/* { dg-additional-options "--param=analyzer-max-macro-alias-depth=1" } */

#include "analyzer-decls.h"

#define O_CREAT 42	/* 0 aliases.  */
#define O_EXCL O_CREAT	/* 1 alias.    */
#define O_RDWR O_EXCL	/* 2 aliases.  */

void test_macro_aliases (void)
{
  __analyzer_dump_named_constant ("O_CREAT"); /* { dg-warning "named constant 'O_CREAT' has value '42'" } */
  __analyzer_dump_named_constant ("O_EXCL"); /* { dg-warning "named constant 'O_EXCL' has value '42'" } */
  __analyzer_dump_named_constant ("O_RDWR"); /* { dg-warning "named constant 'O_RDWR' has unknown value" } */
}
