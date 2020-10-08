// { dg-additional-options "-fmodules-ts -fdump-lang-module-alias" }

import "builtin-5_a.H";

void foo ()
{
  printf ("bob\n");
}

extern "C" int printf (char const *, int);

// { dg-regexp {[^\n]*builtin-5_b.C:10:[0-9]*: error: conflicting declaration of C function 'int printf\(const char\*, int\)'\nIn module [^\n]*builtin-5_a.H, imported at [^\n]*builtin-5_b.C:3:\n[^\n]*builtin-5_a.H:3:[0-9]*: note: previous declaration 'int printf\(const char\*, ...\)'} }

// { dg-final { scan-lang-dump {Read:-1's named merge key \(matched\) function_decl:'::printf'} module } }
