// PR c++/57825
// { dg-do compile { target c++11 } }

template<typename T>
struct target_class
{};

template<typename Class, typename Ret, typename... Args>
struct target_class<Ret (Class::*)(Args...)>
{};

template<typename Class, typename Ret, typename... Args>
struct target_class<Ret (Class::*)(Args...) &>
{};

template<typename Class, typename Ret, typename... Args>
struct target_class<Ret (Class::*)(Args...) &&>
{};
