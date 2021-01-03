// { dg-additional-options -fmodules-ts }

export module anon;
// { dg-module-cmi anon }

export struct foo
{
  enum {bob};
  union 
  {
    int i;
    float f;
  };
};
