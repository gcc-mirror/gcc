/* { dg-options "-fdiagnostics-show-caret -std=c++98" } */
template <typename T>
struct foo {};

foo<foo<int>> i; // { dg-error "12: .>>. should be .> >. within a nested template argument list" }
/* { dg-begin-multiline-output "" }
 foo<foo<int>> i;
            ^~
            > >
   { dg-end-multiline-output "" } */
