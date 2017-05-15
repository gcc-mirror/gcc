// PR c++/49604
// { dg-do compile { target c++11 } }

class MyTable {
public:
    enum Constants : unsigned;
};

enum MyTable::Constants : unsigned {
    LENGTH = 12,
};

int main()
{
    return MyTable::LENGTH;
}
