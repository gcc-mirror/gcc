// Build don't link:
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 1 Sep 2000 <nathan@codesourcery.com>

// bug 127. We ICE'd when given a non-template TYPE_DECL as a template name.

template <class charT>
class basic_string
{
public:
  typedef charT* iterator;
  explicit basic_string ();
  ~basic_string ();
};

void foo () {
  basic_string<char>::iterator<char> p; // ERROR - not a template // ERROR - no type
}
