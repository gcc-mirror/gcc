// PR c++/77948
// { dg-do compile }
// { dg-options "-std=c++11 -std=gnu++11" }

void
foo ()
{
  auto qfp = 1.0q; // { dg-error "unsupported" "" { target { ! has_q_floating_suffix } } }
  auto Qfp = 1.0Q; // { dg-error "unsupported" "" { target { ! has_q_floating_suffix } } }
}
