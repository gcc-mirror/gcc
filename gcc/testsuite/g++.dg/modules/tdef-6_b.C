// { dg-additional-options "-fmodules-ts -fdump-lang-module-alias" }

typedef struct
{
  int __count;
} __mbstate_t;

typedef struct x
{
  int __count;
} X;

import "tdef-6_a.H";

X y;
__mbstate_t x;

// { dg-final { scan-lang-dump {Read:-[0-9]'s named merge key \(matched\) type_decl:'::__mbstate_t'} module } }
