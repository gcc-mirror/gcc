// PR c++/69111
// { dg-do compile { target c++11 } }

template <template <typename> class ...>
struct template_list {};

template <typename T>
struct A
{};

template <typename>
struct B
{
 template <typename T>
 using type = A<T>;
};

template <typename ... Types>
struct C
{
 using type = template_list<B<Types>::template type...>;
};

int main()
{
 return 0;
}
