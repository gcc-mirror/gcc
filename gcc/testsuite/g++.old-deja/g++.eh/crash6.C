// { dg-do assemble  }
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 April 2001 <nathan@codesourcery.com>
// Origin: schmid@snake.iap.physik.tu-darmstadt.de

// Bug 2368. When checking shadowed catchers, we didn't ignore
// template type parms etc, leading to an ICE

template<class CatchType1, class CatchType2>
void call(int& a)
{
  try 
    {
      
    }
  catch (CatchType1&)
    { 
      
    }
  catch (CatchType2&)
    { 
      
    }
}

