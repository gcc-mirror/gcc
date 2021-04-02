// PR c++/96268
// { dg-do compile { target c++20 } }

template <int N>
struct static_string { char chars[N]; /* operator<=> */ };

template <int N>
static_string(char const(&)[N]) -> static_string<N>;

static_string hi = {"hi"};

template <static_string str> struct name {};
using Hi = name<{"hi"}>;
