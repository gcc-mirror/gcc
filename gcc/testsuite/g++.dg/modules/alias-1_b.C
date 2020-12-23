// { dg-additional-options "-fmodules-ts -fdump-lang-module -isystem [srcdir]" }

// Alias at the header file.  We have one CMI file
import "alias-1_a.H";
import <alias-1_a.H>;

int main ()
{
  frob ();
}

// { dg-final { scan-lang-dump-times {CMI is } 1 module } }
