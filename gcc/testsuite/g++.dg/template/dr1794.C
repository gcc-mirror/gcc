// DR 1794 - template keyword and alias templates.
// { dg-do compile { target c++11 } }

template<template<typename> class Template>
struct Internal {
  template<typename Arg>
  using Bind = Template<Arg>;
};

template<template<typename> class Template, typename Arg>
using Instantiate = Template<Arg>;

template<template<typename> class Template, typename Argument>
using Bind = Instantiate<Internal<Template>::template Bind, Argument>;
