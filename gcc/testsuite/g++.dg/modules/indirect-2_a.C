// { dg-module-do run }

export module foo;
// { dg-module-bmi foo }

namespace foo
{
  export template<int I> int frob ()
  {
    return I;
  }
}

