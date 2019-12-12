// PR c++/80916
// { dg-options "-Os -Wunused" }

struct j {
  virtual void dispatch(void *) {}
};
template <typename>
struct i : j {
  void dispatch(void *) {} // warning: 'void i< <template-parameter-1-1> >::dispatch(void*) [with <template-parameter-1-1> = {anonymous}::l]' declared 'static' but never defined [-Wunused-function]
};
namespace {
  struct l : i<l> {};
}
void f(j *k) {
  k->dispatch(0);
}
