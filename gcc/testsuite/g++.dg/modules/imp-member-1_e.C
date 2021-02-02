// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks-alias" }
import B;

void fn ()
{
  C c; // created C::C implicitly
}

void fn2 ()
{
  D d; // merges implicit C::C
}

// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(matched\) function_decl:'::C@A:.::__ct '\n} module } }
// { dg-final { scan-lang-dump-not {Adding implicit member '::C@A:.::__ct @B:.} module } }
