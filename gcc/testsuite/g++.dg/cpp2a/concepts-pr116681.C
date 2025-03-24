// PR c++/116681
// { dg-do compile { target c++20 } }
// { dg-additional-options "-ftime-report" }
// { dg-allow-blank-lines-in-output 1 }
// { dg-prune-output "Time variable" }
// { dg-prune-output "k" }
// { dg-prune-output "\[0-9\]+%" }

template < int> using __conditional_t = int;
template < typename _Iter >
concept random_access_iterator = requires { new _Iter; };
template < typename _Iterator > struct reverse_iterator {
  using iterator_concept = __conditional_t< random_access_iterator< _Iterator >>;
};
void RemoveBottom()
{
  int iter;
  for (reverse_iterator< int > iter;;)
      ;
}
