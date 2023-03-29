// PR c++/109030
// { dg-do compile { target c++11 } }

struct foo { };

struct __as_receiver {
  foo empty_env;
};
void sched(foo __fun) noexcept(noexcept(__as_receiver{__fun})) { }
