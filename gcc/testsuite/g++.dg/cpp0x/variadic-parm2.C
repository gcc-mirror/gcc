// PR c++/66519
// { dg-do compile { target c++11 } }

template<typename F, typename... Tp>
void a(F f, Tp... args, decltype(f(args...)) = 1) {}

template<typename F, typename... Tp>
void a(F, Tp...) {}

void b(void) {
    a([]{});
}
