// PR c++/57254
// { dg-require-effective-target c++11 }

struct foo {
    template<typename T>
    void bar(T) &;

    template<typename T>
    void bar(T) &&;
};

template<typename T>
void foo::bar(T) & {}

template<typename T>
void foo::bar(T) && {}

int main()
{
  foo f;
  f.bar(0);
}
