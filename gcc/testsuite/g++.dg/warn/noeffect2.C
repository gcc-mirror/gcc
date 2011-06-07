// { dg-do compile }
// { dg-options "-Wall" }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 18 Aug 2003 <nathan@codesourcery.com>
// Origin PR 11945 gerald@pfeifer.com

// PR 11945 inconsistent warnings

extern "C" void FormatDisk();
  template <class T>
  struct C {
    C(){ FormatDisk(), 0; }  // { dg-warning "right operand of comma" "" }
  };
  template struct C<int>; // { dg-message "required" }
  template <class T>
  void f() { FormatDisk(), 0; } // { dg-warning "right operand of comma" "" }
  template void f<int> (); // { dg-message "required" }
void g() { FormatDisk(), 0; } // { dg-warning "right operand of comma" "" }

