// Build don't link: 
// GROUPS passed old-abort
class A
{
  public:
    class B
    {
      public:
        virtual ~B();
    };
};

template<int I>
class C
{
  public:
    class B
    : public A::B
    {
    }; // bug2.cc:18: Internal compiler error 233.
       // bug2.cc:18: Please submit a full bug report to `bug-g++@prep.ai.mit.edu'.
};

C<0> c;
