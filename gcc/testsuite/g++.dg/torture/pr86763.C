// { dg-do run { target { *-*-linux* } } }
// { dg-additional-options "-fschedule-insns2 -fstrict-aliasing" }
// { dg-additional-options "-lrt" }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }

#include <cstdint>
#include <cassert>
#include <time.h>
struct ID {
  uint64_t value;
};
uint64_t value(ID id) { return id.value; }
uint64_t gen { 1000 };
struct Msg {
  uint64_t time;
  ID id;
};
struct V {
  V() { }
  V(Msg const & msg) : msg(msg) { }
  Msg & get() { return msg; }
  Msg msg;
  char pad[237 - sizeof(Msg)];
};
struct T : V { using V::V; };
Msg init_msg() {
  Msg msg;
  timespec t;
  clock_gettime(CLOCK_REALTIME, &t);
  msg.time = t.tv_sec + t.tv_nsec;
  msg.id.value = ++gen;
  return msg;
}
int main() {
  T t;
  t = init_msg();
  assert(value(t.get().id) == 1001);
}
