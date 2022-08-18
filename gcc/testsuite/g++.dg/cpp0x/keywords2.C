// { dg-do compile { target c++98_only } }
// { dg-options "-Wc++11-compat" }

// Validate suppression of -Wc++11-compat diagnostics.
#pragma GCC diagnostic ignored "-Wc++11-compat"
int alignof;
int alignas;
int constexpr;
int decltype;
int noexcept;
int nullptr;
int static_assert;
int thread_local;
int _Alignas;
int _Alignof;
int _Thread_local;
