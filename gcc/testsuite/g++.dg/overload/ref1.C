// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 5 Mar 2004 <nathan@codesourcery.com>

// Origin: schmid@snake.iap.physik.tu-darmstadt.de
// Bug 14397: Bogus access error.

struct S { 
    S (int); 
    S(S const&); 
  private: 
    S(S&); 
}; 
 
S foo() 
{ 
  int result = 0;
  
  S s ((0,S (result)));
  
  return S (result); 
} 
