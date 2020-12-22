// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks" }
import B;

void fn ()
{
  C c;  // creates C::C implicitly (we never read B's version)
}

// { dg-final { scan-lang-dump-not {Read:-1's named merge key \([a-z]*\) function_decl:'::C@A:.::__ct '\n} module } }
// { dg-final { scan-lang-dump-not {Adding implicit member '::C@A:.::__ct @B:.} module } }
