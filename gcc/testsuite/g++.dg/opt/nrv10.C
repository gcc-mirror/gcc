// PR c++/25979
// Bug: we were eliding too many temporaries, so that a1 was used
// as both 'a' and 'x' in the second operator+.
// { dg-do run }

struct A
{
    A() : data1_(0), data2_(0) {}
    A(int i, int j) : data1_(i), data2_(j) {}
    A operator+(int);
    friend A operator+(int, const A&);
    ~A() {}
//private:
    int data1_;
    int data2_;
};

extern bool x;

extern "C" void abort ();

int main()
{
    A a1(1,2);
    a1 = (x ? a1 + 3 : 3 + a1);
    if (a1.data1_ != 3 || a1.data2_ != 2)
      abort ();
}

bool x = false;

A
A::operator+(int i)
{
    A a;
    a = *this;
    a.data2_ = i;
    return a;
}

A
operator+(int i, const A& x)
{
    A a;
    a = x;
    a.data1_ = i;
    return a;
}
