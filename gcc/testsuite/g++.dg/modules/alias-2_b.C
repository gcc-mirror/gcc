// { dg-additional-options "-fmodules-ts -fdump-lang-module -isystem [srcdir]/sys" }

// These find different headers
import "alias-2_a.H";
import <alias-2_a.H>;

int main ()
{
  frob ();
  frob (1);
}

// { dg-final { scan-lang-dump-times {CMI is} 2 module } }
