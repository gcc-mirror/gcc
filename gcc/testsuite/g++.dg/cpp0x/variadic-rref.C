// { dg-options "-std=c++11" }
// PR c++/33939
template<typename T>
struct refs_only;

template<typename T>
struct refs_only<T &>
{};

template<typename T>
refs_only<T> foo( T && t)
{
    return refs_only<T>();
}

template<typename... T>
struct va_refs_only;

template<typename T>
struct va_refs_only<T>
  : refs_only<T>
{};

template<typename... T>
va_refs_only<T...> bar( T &&... t)
{
    return va_refs_only<T...>();
}

int main()
{
    int j = 0;
    foo(j);
    bar(j); // error: invalid use of incomplete type 'struct refs_only<int>'
}

