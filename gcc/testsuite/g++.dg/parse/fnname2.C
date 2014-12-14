// PR c++/64248

class A
{
public:
    A(const char* str) {};
};

class B
{
public:
    B(A a) {};
};

int main()
{
   B b(A(__func__));
   return 0;
}
