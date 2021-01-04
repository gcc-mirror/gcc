// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks-alias-uid" }
module;
#include <stdarg.h>
export module builtins;
// { dg-module-cmi builtins }

export inline unsigned length (char const *ptr)
{
  return __builtin_strlen (ptr);
}

export inline int count (int a, ...)
{
  int c = 0;

  va_list args;
  va_start (args, a);
  while (va_arg (args, char *))
    c++;
  va_end (args);

  return c;
}

// { dg-final { scan-lang-dump-not {Cluster members:\n  \[0\]=decl declaration '::__builtin_strlen'\n  \[1\]=binding '::__builtin_strlen'\n} module } }
// { dg-final { scan-lang-dump {Wrote GMF:-[0-9]* function_decl:'::__builtin_strlen'@builtins} module } }
// { dg-final { scan-lang-dump {Writing:-[0-9]*'s named merge key \(decl\) function_decl:'::__builtin_strlen'} module } }
// { dg-final { scan-lang-dump-not {Writing tree:-[0-9]* function_decl:'__builtin_strlen'\(strlen\)} module } }

// The implementation details of va_list's are target-specific.
// Usually one of two patterns though
// { dg-final { scan-lang-dump-not { Cluster members:\n  \[0\]=decl declaration '::__builtin_va_list'\n  \[1\]=binding '::__builtin_va_list'\n} module { target i?86-*-linux* x86_64-*-linux* } } }
// { dg-final { scan-lang-dump {Wrote GMF:-[0-9]* type_decl:'::__builtin_va_list'@builtins} module { target x86_64-*-linux* } } }
// { dg-final { scan-lang-dump {Writing:-[0-9]*'s named merge key \(decl\) type_decl:'::__builtin_va_list'} module { target x86_64-*-linux* } } }

// { dg-final { scan-lang-dump {Writing:-1's named merge key \(decl\) type_decl:'::__gnuc_va_list'} module { target i?86-*-linux* *-*-darwin* } } }
// { dg-final { scan-lang-dump {Wrote GMF:-3 type_decl:'::__gnuc_va_list'@builtins} module { target i?86-*-linux* *-*-darwin* } } }

// { dg-final { scan-lang-dump {Wrote fixed:[0-9]* record_type:'__va_list'} module { target aarch64*-*-linux* } } }
// { dg-final { scan-lang-dump {Wrote fixed:[0-9]* pointer_type:'::__builtin_va_list'} module { target powerpc*-*-linux* } } }

// { dg-final { scan-lang-dump-not { Cluster members:\n  \[0\]=decl declaration '::va_list'\n  \[1\]=binding '::va_list'\n} module } }
// { dg-final { scan-lang-dump {Wrote GMF:-[0-9]* type_decl:'::va_list'@builtins} module } }
// { dg-final { scan-lang-dump {Writing:-[0-9]*'s named merge key \(decl\) type_decl:'::va_list'} module } }
