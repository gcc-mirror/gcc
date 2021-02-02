// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks-alias" }
import B;

void fn ()
{
  D d; // reads in C::C implicit ctor
}

// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(new\) function_decl:'::C@A:.::__ct '\n} module } }
// { dg-final { scan-lang-dump {Adding implicit member '::C@A:.::__ct @B:.} module } }
