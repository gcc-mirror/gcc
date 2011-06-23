// PR c++/49507
// { dg-options -std=c++0x }

template<typename T>
struct ConcretePoolKey
{
        virtual ~ConcretePoolKey();
};

template<typename T>
ConcretePoolKey<T>::~ConcretePoolKey() = default;

int main()
{
        ConcretePoolKey<int> foo;
}
