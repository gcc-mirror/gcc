// PR c++/51586

union U
{
  union U { int i; };		// { dg-error "same name" }
};
