// PR c++/105268
// { dg-do compile { target concepts } }

template<typename> concept C_one = true;
template<typename...> concept C_many = true;

template<bool B> struct S { };

template<typename T = S<C_one<int>>> void f();
template<typename T = S<C_many<int>>> void g();

void
fn (auto a = S<C_one<int>>{})
{
}

void
fn2 (auto a = S<C_many<int>>{})
{
}
