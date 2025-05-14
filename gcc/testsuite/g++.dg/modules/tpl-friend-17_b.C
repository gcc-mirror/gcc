// PR c++/118920
// { dg-additional-options "-fmodules" }

#include "tpl-friend-17.h"
import M;

int main() {
  // instantiating shared_ptr<int> should find previously generated
  // out_ptr_t template from the unique_ptr<int> instantiation
  foo<int>();
}
