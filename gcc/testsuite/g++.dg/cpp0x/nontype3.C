// PR c++/53009
// { dg-do compile { target c++11 } }

template<typename T, T> class function_proxy;

template<typename Return, typename Obj, Return(*func)(Obj)>
struct function_proxy<Return(*)(Obj), func>
{
    static void wrapper(){ }
};

template<typename CT, CT> class member_helper;

template<typename Class, void(Class::*fun)()>
struct member_helper<void(Class::*)(), fun>
{
    static void as_free(Class& obj){ }

    static void worker(){
        (void) function_proxy<decltype(&as_free), &as_free>::wrapper;
    }
};

struct Test
{
    void test(){ }
};

int main()
{
    member_helper<decltype(&Test::test), &Test::test>::worker();
}
