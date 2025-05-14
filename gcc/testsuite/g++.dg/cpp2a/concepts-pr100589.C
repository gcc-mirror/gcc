// PR c++/100589
// { dg-do compile { target c++20 } }

template<class T>
concept false_concept = false;

false_concept auto f() -> int; // { dg-error "'f' \[^\r\n\]* trailing return type has constrained 'auto'" }

using type = false_concept auto() -> int; // { dg-error "invalid use of constrained 'auto' type" }
