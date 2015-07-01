/* { dg-options "-std=c++11" } */
void gg();
static __typeof(gg) __gthrw_gg __attribute__((__weakref__("gg")));

template<typename R,typename... A>
struct data {
 template<typename Y,typename X>
 data(Y& y,R(X::*f)(A...));
};

template<typename Y,typename X,typename R,typename... A>
data<R,A...> make_data(Y& y,R(X::*f)(A...)) {
 return data<R,A...>(y,f);
}

void global(data<void>);

struct test {
 void bar() {}
 void doit() { global(make_data(*this,&test::bar)); }
};

