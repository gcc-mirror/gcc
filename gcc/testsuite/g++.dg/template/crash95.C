// PR c++/43705

template < typename > struct S
{
  template < > struct S < int > // { dg-error "explicit|specialization|template|parameter" }
  {
    S(int);
  };
};

S < int > s(0); // { dg-error "no matching|too many initializers" }
