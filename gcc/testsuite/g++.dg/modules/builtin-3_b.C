// { dg-additional-options "-fmodules-ts -fdump-lang-module-alias" }
import builtins;

int main ()
{
  length ("");
  count (1, "", "", nullptr);
}

// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(matched\) function_decl:'::__builtin_strlen'} module } }
// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(matched\) type_decl:'::__builtin_va_list'} module { target { x86_64-*-linux* } } } }
// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(new\) type_decl:'::va_list'} module } }
// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(new\) type_decl:'::__gnuc_va_list'} module } }
