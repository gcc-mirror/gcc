// PR c++/15938
// { dg-do compile }
// { dg-options "" }

typedef union
{
  struct { int i; };
  struct { char c; };
} A;

A a = { 0 };
A b = {{ 0 }};
A c = {{{ 0 }}};    // { dg-error "braces" "" { target c++98_only } }
A d = {{{{ 0 }}}};  // { dg-error "braces" }
