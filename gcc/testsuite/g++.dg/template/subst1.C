// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 16 Sep 2002 <nathan@codesourcery.com>

// PR 7718. ICE.

template <typename OBJECT>
void default_initializer(const OBJECT &) { }


template <typename OBJECT, void init_function(const OBJECT &)>
class cContainer {
  public:
  template <typename INITIALIZER>
  void Add(const INITIALIZER &initializer) {
    init_function(initializer);
  }
};

int main() {
  cContainer<int, default_initializer<int> > c;
  
  c.Add<int>(42);
  
  return 0;
}
