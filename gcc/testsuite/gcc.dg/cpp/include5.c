/* Test #include directives with macros expanding to empty.  */

#define EMPTY_OBJ
#define EMPTY_FUNC()

#include <stddef.h> EMPTY_OBJ
#include <stddef.h> EMPTY_FUNC()
#include "stddef.h" EMPTY_OBJ
#include "stddef.h" EMPTY_FUNC()
