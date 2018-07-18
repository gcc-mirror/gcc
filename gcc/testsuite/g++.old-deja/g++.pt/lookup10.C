// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation
// Contributed by Nathan Sidwell 3 July 2000 <nathan@codesourcery.com>
// We'd get confused entering a namespace via an alias

namespace Outer {
  namespace Render_Real {
    typedef void Type;
  }

  namespace Core_Real {}  
  namespace Core = Core_Real;

  namespace Core_Real {
    template<class T> void Foo (T *) {}
  }

  template<> void Core::Foo<> (Render_Real::Type *) {} // { dg-error "" "" { target c++98_only } }
}  
