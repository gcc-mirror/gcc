// PR c++/116327 - ICE in coroutine with parameter preview on lambda with captures

#include <coroutine>

struct coroutine{
  struct promise_type{
    promise_type(const auto &...){}
    std::suspend_never initial_suspend(){ return {}; }
    std::suspend_always final_suspend()noexcept{ return {}; }
    void unhandled_exception(){}
    coroutine get_return_object(){ return {}; }
    void return_value(int)noexcept{}
  };
};

int main(){
  auto f = [a=0](auto) -> coroutine {
    co_return 2;
  };
  f(0);
  return 0;
}
