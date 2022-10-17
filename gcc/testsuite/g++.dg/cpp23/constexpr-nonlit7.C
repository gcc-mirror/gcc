// PR c++/104994
// CWG2552
// { dg-do compile { target c++14 } }

constexpr bool foo () { extern thread_local int t; return true; }
static constexpr bool a = foo ();
