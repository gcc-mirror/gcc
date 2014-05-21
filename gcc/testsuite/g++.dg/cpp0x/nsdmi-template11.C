// PR c++/58930
// { dg-do compile { target c++11 } }

struct SampleModule
{
  explicit SampleModule (int);
};

template < typename >
struct BaseHandler
{
  SampleModule module_ { 0 };
};

BaseHandler<int> a;
