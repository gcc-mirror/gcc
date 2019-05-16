// { dg-additional-options -fmodules-ts }

export module anon;
// { dg-module-bmi anon }

export struct foo
{
  enum {bob};
  union 
  {
    int i;
    float f;
  };
};
