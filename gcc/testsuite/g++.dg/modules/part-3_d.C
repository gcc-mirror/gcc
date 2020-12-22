// { dg-additional-options "-fmodules-ts -fdump-lang-module-alias" }
import foo;

int main ()
{
  int r = frob (75);
  return !(r == -75);
}

// { dg-final { scan-lang-dump { Read:-[0-9]'s named merge key \(unique\) function_decl:'::frob'} module } }
// { dg-final { scan-lang-dump {>  Read:-[0-9]'s named merge key \(unique\) function_decl:'::foo'} module } }
