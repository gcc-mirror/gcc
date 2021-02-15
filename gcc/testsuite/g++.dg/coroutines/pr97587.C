//  { dg-do run }

#include<cassert>
#include<coroutine>

int *parameter_addr_in_promise_ctor;

struct return_object{
    struct promise_type{

        promise_type(int &parameter)
        {
            parameter_addr_in_promise_ctor = &parameter;
        }

        return_object get_return_object(){ return {}; }

        void return_void(){}

        auto initial_suspend(){ return std::suspend_never{}; }
        auto final_suspend() noexcept { return std::suspend_never{}; }
        void unhandled_exception(){}
    };
};
return_object coroutine(int parameter = 42){
    assert(&parameter == parameter_addr_in_promise_ctor);
    co_return;
}

int main(int,char**){
    coroutine();
}
