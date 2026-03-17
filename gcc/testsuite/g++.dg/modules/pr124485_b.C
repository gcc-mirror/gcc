// PR c++/124485
// { dg-additional-options "-fmodules -Wno-global-module" }

import foo;
struct Type {
  virtual ~Type() {}
};
