// { dg-additional-options "-fmodules-ts -fdump-lang-module -isystem [srcdir]/sys" }

// These find different headers with different controlling macros, so are different
import "alias-2_a.H";
import <alias-2_a.H>;

int main ()
{
  frob ();
  frob (1);
}

// { dg-final { scan-lang-dump {Controlling macro is ALIAS_2_A_SYS} module } }
// { dg-final { scan-lang-dump {Controlling macro is ALIAS_2_A} module } }
