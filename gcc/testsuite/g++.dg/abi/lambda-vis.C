// { dg-do compile { target c++17 } }
// { dg-options "-fno-inline" }

template<typename T> int sfoo (T); // { dg-warning "used but never defined" }
template<typename T> int gfoo (T); // OK, but not completable
template<typename T> int ifoo (T); // OK
template<typename T> struct Wrapper {};
template<typename T> Wrapper<T> capture (T &&) {return Wrapper<T> ();}

static int svar = sfoo (capture ([]{}));

int gvar = gfoo (capture ([]{}));

inline int ivar = ifoo (capture ([]{}));

// { dg-final { scan-assembler {_?_Z7captureINL4svarMUlvE_EE7WrapperIT_EOS2_:} } }
// { dg-final { scan-assembler {_?_Z7captureIN4gvarMUlvE_EE7WrapperIT_EOS2_:} } }
// { dg-final { scan-assembler {_?_Z7captureIN4ivarMUlvE_EE7WrapperIT_EOS2_:} } }

// Calls to the foos are emitted.
// { dg-final { scan-assembler {call[ \t]*_?_Z4sfooI7WrapperINL4svarMUlvE_EEEiT_} { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler {call[ \t]*_?_Z4gfooI7WrapperIN4gvarMUlvE_EEEiT_} { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler {call[ \t]*_?_Z4ifooI7WrapperIN4ivarMUlvE_EEEiT_} { target { i?86-*-* x86_64-*-* } } } }
