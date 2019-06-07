// { dg-additional-options "-fmodules-ts -fdump-lang-module-eh -isystem [srcdir]/sys" }

// These find different headers with the same controlling macro, so
// are the same
import <alias-3_a.H>;
import "alias-3_a.H";

int main ()
{
  frob (); //  { dg-error "not declared" }
  frab ();
}

// { dg-final { scan-lang-dump {Controlling macro is ALIAS_3_A} module } }
// { dg-final { scan-lang-dump {[^\n]alias-3_a.H is an alias of [^\n]*sys/alias-3_a.H} module } }
