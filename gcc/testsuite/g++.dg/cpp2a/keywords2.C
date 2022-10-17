// { dg-do compile { target c++17_down } }
// { dg-options "-Wc++20-compat" }

// Validate suppression of -Wc++20-compat diagnostics.
#pragma GCC diagnostic ignored "-Wc++20-compat"
int constinit;
int consteval;
int requires;
int concept;
int co_await;
int co_yield;
int co_return;
int char8_t;
