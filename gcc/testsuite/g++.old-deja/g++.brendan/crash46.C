// { dg-do assemble  }
// GROUPS passed old-abort
class Rational {
public:
    Rational(int v): value(v)
	{  }

    int value;
};
typedef Rational __Rational;

extern int operator>(const Rational&, const Rational&);

class V {
public:
    class Rational {
    public:
	static int x(const __Rational& value);
    };
};

int
V::Rational::x(const __Rational& value)
{
    return value > 0;
}
