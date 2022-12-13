// PR c++/104066
// { dg-do compile { target c++20 } }

constinit void (*p)() = nullptr;
constinit void (*pp)() = nullptr;
void fn();
constinit void (&r)() = fn;

extern constinit long (* const syscall_reexported) (long, ...);

constinit void bad (); // { dg-error ".constinit. on function return type is not allowed" }
constinit void bad () { } // { dg-error ".constinit. on function return type is not allowed" }
