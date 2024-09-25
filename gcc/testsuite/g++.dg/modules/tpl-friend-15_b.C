// PR c++/114950
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi M:b }

module M:b;
extern "C++" {
  #include "tpl-friend-15.h"
}
