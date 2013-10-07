// PR c++/58503
// { dg-require-effective-target c++11 }
// { dg-options "-fpermissive -w" }

struct c { };

template<int> void foo()
{
  for (auto i : c()) { }
}

c* begin(const c&);
c* end(const c&);

template void foo<1>(); 
