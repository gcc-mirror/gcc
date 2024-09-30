// PR c++/114950
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi M:a }

module M:a;
extern "C++" {
  #include "tpl-friend-15.h"
}
