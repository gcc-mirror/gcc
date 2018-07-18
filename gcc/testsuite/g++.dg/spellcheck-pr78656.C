// { dg-options "-fdiagnostics-show-caret" }

#include <memory>

void* allocate(std::size_t n)
{
  return std::allocate<char>().allocate(n); // { dg-error ".allocate. is not a member of .std." }
  // { dg-message "suggested alternative: .allocator." "" { target *-*-* } .-1 }
  /* { dg-begin-multiline-output "" }
   return std::allocate<char>().allocate(n);
               ^~~~~~~~
     { dg-end-multiline-output "" } */ 
  /* { dg-begin-multiline-output "" }
   return std::allocate<char>().allocate(n);
               ^~~~~~~~
               allocator
     { dg-end-multiline-output "" } */

  // Various errors follow that we don't care about; suppress them:
  // { dg-excess-errors "7: " }
}

void* test_2(std::size_t n)
{
  return std::alocator<char>().allocate(n); // { dg-error ".alocator. is not a member of .std." }
  // { dg-message "suggested alternative: .allocator." "" { target *-*-* } .-1 }
  /* { dg-begin-multiline-output "" }
   return std::alocator<char>().allocate(n);
               ^~~~~~~~
     { dg-end-multiline-output "" } */ 
  /* { dg-begin-multiline-output "" }
   return std::alocator<char>().allocate(n);
               ^~~~~~~~
               allocator
     { dg-end-multiline-output "" } */

  // Various errors follow that we don't care about; suppress them:
  // { dg-excess-errors "25: " }
}
