// { dg-do compile }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 15 Dec 2001 <nathan@codesourcery.com>

// PR 2645

template <typename T>
struct call_traits
{
  public:
  typedef T type_less_spec;
};

template <typename T>
struct call_traits<T&>
{
  typedef T type_more_spec;
};


int main()
{
  int num;
  
   // Two typedefs lead to the instant. of the less spec. ("wrong") template
  typedef int& r_type;
  typedef const r_type cr_type;
  call_traits<cr_type>::type_less_spec var  = num; // { dg-error "" "" }
  
   // The explicit type leads to the instantiation of the "correct" one
  call_traits<const int&>::type_more_spec var2 = num;
  
   // As happen with a single typedef!
  typedef const int& std_cr_type;
  call_traits<std_cr_type>::type_more_spec var3 = num;
  
  
   // As happen, indeed, without the cv-qualifier
  call_traits<r_type>::type_more_spec var4;
}
