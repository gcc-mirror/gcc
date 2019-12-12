// PR c++/91844 - Implement CWG 2352, Similar types and reference binding.
// { dg-do compile { target c++11 } }

template<typename T> int f (const T *const &); // 1
template<typename T> int f (T *const &); // 2
template<typename T> int f (T *); // 3

/* There's an ambiguity: (2) is a better match than (1) because
   (1) requires a qualification conversion whereas (2) doesn't, but
   (2) and (3) are indistinguishable conversion sequences.  */
   
void
g (int *p, const int *q, const int *const r)
{
  f (p); // { dg-error "call of overloaded" }
  f (q);
  f (r);
}
