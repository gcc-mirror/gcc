// Build don't link:  
// XFAIL, doesn't.

class MyInt
{
public:
        MyInt(int = 0) {}
        operator int() const {return 2;}
};

bool operator==(const MyInt& a, const int& b)
{
        return (int)a == b;
}

bool operator==(const MyInt& a, const MyInt& b)
{
        return (int)a == (int)b;
}

bool f()
{
        return 3 == MyInt();
}
