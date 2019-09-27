// PR c++/91844 - Implement CWG 2352, Similar types and reference binding.
// { dg-do compile { target c++11 } }

template<typename T> int f (const T *const &); // (1)
template<typename T> int f (T *const &); // (2)
template<typename T> int f (T *); // (3)

/* Before CWG 2352, (2) was a better match than (1), but (2) and (3) were
   equally good, so there was an ambiguity.  (2) was better than (1) because
   (1) required a qualification conversion whereas (2) didn't.  But with this
   CWG, (1) no longer requires a qualification conversion, because the types
   "const int* const" and "int *" are now considered reference-related and we
   bind directly, and (1) is more specialized than (2).  And (1) is also a
   better match than (3).  */

void
g (int *p, const int *q, const int *const r)
{
  f (p); // calls (1)
  f (q); // calls (1)
  f (r); // calls (1)
}
