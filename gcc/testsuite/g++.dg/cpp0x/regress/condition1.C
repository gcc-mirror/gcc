// PR c++/47950
// { dg-options -std=c++0x }

template <typename T> struct empty
{
   // allow success case to build (not relevant to bug)
   operator bool() { return true; }
};

template <typename T> struct from_int
{
   from_int(int) {}

   // allow success case to build (not relevant to bug)
   operator bool() { return true; }
};

template <typename T>
from_int<T> via_function(T v)
{
   return from_int<T>(v);
}

template <typename T>
void f()
{
   // ********* this section compiles ***********

   // these plain initializers work fine
   from_int<int> a = 7;
   from_int<int> b = from_int<int>(7);
   empty<int>    c = empty<int>();
   from_int<T> ta = 7;
   from_int<T> tb = from_int<T>(7);
   empty<T>    tc = empty<T>();

   // these dependent condition decls work fine
   if (empty<T> x = empty<T>())
      ;
   if (from_int<T> x = 7)
      ;
   if (from_int<T> x = from_int<T>(7))
      ;
   if (from_int<T> x = via_function(T()))
      ;

   // this non-dependent condition decl using conversion works fine
   if (from_int<int> x = 7)
      ;

   // these non-dependent condition decls using conversion or braced-
   // initialization work fine (in c++0x mode only course)
   #if __GXX_EXPERIMENTAL_CXX0X__
   if (empty<int> x {})
      ;
   if (from_int<int> x {7})
      ;
   #endif

   // ********** this section fails in C++0x ***********

   // the following non-dependent condition decls cause an assertion
   // failure in
   //
   //   tsubst_copy_and_build, at cp/pt.c:13370
   //
   // in C++0x mode
   //
   if (empty<int> x = empty<int>())
      ;
   if (from_int<int> x = from_int<int>(7))
      ;
   if (from_int<int> x = via_function(7))
      ;
}

int main()
{
   f<int>();
}
