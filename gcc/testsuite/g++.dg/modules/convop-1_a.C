// { dg-module-do run }

export module frob;
// { dg-module-bmi "frob" }

export struct A
{
  operator int () 
  {
    return 0;
  }
};
