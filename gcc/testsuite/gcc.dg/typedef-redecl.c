/* Redeclaration of typedef (invalid but accepted in system headers)
   causes ICE; PR 13656.  Test case by Richard Sandiford <rsandifo@redhat.com>,
   reduced from glibc.  */

#include "typedef-redecl.h"
x a;
