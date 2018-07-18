// PR c++/86190
// { dg-options "-Wsign-conversion -Wsign-compare" }

typedef unsigned long size_t;

struct vector {
  typedef size_t size_type;
  size_type size();
};

bool func(vector vec, int var)
{
  return vec.size() < static_cast<size_t>(var); // { dg-bogus "may change" }
}
