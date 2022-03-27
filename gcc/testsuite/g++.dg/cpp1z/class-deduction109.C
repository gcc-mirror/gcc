// PR c++/103943
// { dg-do compile { target c++17 } }

template<typename R, typename...AA> struct F0 { //OK
    R(*fun_ptr)(AA...);
};
template<typename R, typename...AA> struct F1 { //OK
    R(*fun_ptr)(AA...);
    F1(R(*fun_ptr)(AA...)) : fun_ptr(fun_ptr) {}
};
template<typename R, typename...AA> struct F2 { //OK
    R(*fun_ptr)(AA...);
    using fun_ptr_t = decltype(fun_ptr);
    F2(fun_ptr_t fun_ptr) : fun_ptr(fun_ptr) {}
};
template<typename R, typename...AA> struct F3 {
    R(*fun_ptr)(AA...);
//  using fun_ptr_t = decltype(fun_ptr); //OK as in F2
    using fun_ptr_t = decltype(F3::fun_ptr); //ICE: Segmentation fault
//  using fun_ptr_t = decltype(F3<R, AA...>::fun_ptr); //ICE: Segmentation fault
    F3(fun_ptr_t fun_ptr) : fun_ptr(fun_ptr) {}
};
template<typename R, typename...AA> struct F4 {
    static R fun_not_implemented(AA...);
//  using fun_ptr_t = decltype(&fun_not_implemented); //OK
    using fun_ptr_t = decltype(&F4::fun_not_implemented); //OK with aggregate initialization (no ctor)
//  using fun_ptr_t = decltype(&F4<R, AA...>::fun_not_implemented); //OK with aggregate initialization (no ctor)
    fun_ptr_t fun_ptr;
};
template<typename R, typename...AA> struct F5 { //OK
    static R fun_not_implemented(AA...);
    using fun_ptr_t = decltype(&fun_not_implemented);
    fun_ptr_t fun_ptr;
    F5(fun_ptr_t fun_ptr) : fun_ptr(fun_ptr) {}
};
template<typename R, typename...AA> struct F6 {
    static R fun_not_implemented(AA...);
//  using fun_ptr_t = decltype(&fun_not_implemented); //OK as in F5
    using fun_ptr_t = decltype(&F6::fun_not_implemented); //ICE: in build_qualified_name, at cp/tree.c:2238
//  using fun_ptr_t = decltype(&F6<R, AA...>::fun_not_implemented); //ICE: in build_qualified_name, at cp/tree.c:2238
    fun_ptr_t fun_ptr;
    F6(fun_ptr_t fun_ptr) : fun_ptr(fun_ptr) {}
};
template<typename R, typename...AA> F0(R(*fun_ptr)(AA...)) -> F0<R, AA...>;
template<typename R, typename...AA> F1(R(*fun_ptr)(AA...)) -> F1<R, AA...>;
template<typename R, typename...AA> F2(R(*fun_ptr)(AA...)) -> F2<R, AA...>;
template<typename R, typename...AA> F3(R(*fun_ptr)(AA...)) -> F3<R, AA...>;
template<typename R, typename...AA> F4(R(*fun_ptr)(AA...)) -> F4<R, AA...>;
template<typename R, typename...AA> F5(R(*fun_ptr)(AA...)) -> F5<R, AA...>;
template<typename R, typename...AA> F6(R(*fun_ptr)(AA...)) -> F6<R, AA...>;

int fun(int a) {
    return a + 1;
}
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
void test() {
    auto f0 = F0{&fun}; //OK
    auto f1 = F1{&fun}; //OK
    auto f2 = F2{&fun}; //OK
    auto f3 = F3{&fun}; //ICE: Segmentation fault
    auto f4 = F4{&fun}; //OK
    auto f5 = F5{&fun}; //OK
    auto f6 = F6{&fun}; //ICE: in build_qualified_name, at cp/tree.c:2238
}
