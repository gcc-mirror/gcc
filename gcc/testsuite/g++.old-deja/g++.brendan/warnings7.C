// { dg-do assemble  }
// { dg-options "-O2 -Wall" }
// GROUPS passed warnings
template<class T>
class X {
public:
    void kill(unsigned i)
	{ vec[i].~T(); }
    T *vec;
};

class Y { };

void
x()
{
    X<int> x;
    x.kill(0);
    X<Y> y;
    y.kill(0);
}
