// { dg-additional-options -fmodules-ts }
module;
#include <stdarg.h>
export module builtins;
// { dg-module-cmi builtins }

export {
  using ::va_list;
}
