// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks-alias" }
import B;

void fn ()
{
  D d; // reads in C::C implicits
}

// { dg-final { scan-lang-dump-times {Read:-[0-9]*'s named merge key \(new\) function_decl:'::C@A:.::__dt '\n} 1 module } }
// { dg-final { scan-lang-dump-times {Adding implicit member '::C@A:.::__dt @B:.} 1 module } }
// { dg-final { scan-lang-dump-times {Read:-[0-9]*'s named merge key \(new\) function_decl:'::C@A:.::__ct '\n} 3 module } }
// { dg-final { scan-lang-dump-times {Adding implicit member '::C@A:.::__ct @B:.} 3 module } }
// { dg-final { scan-lang-dump-times {Read:-[0-9]*'s named merge key \(new\) function_decl:'::C@A:.::operator='\n} 2 module } }
// { dg-final { scan-lang-dump-times {Adding implicit member '::C@A:.::operator=@B:.} 2 module } }
