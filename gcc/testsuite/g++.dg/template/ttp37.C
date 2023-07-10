// PR c++/110523

template<template<class> class>
class basic_json;

template<class>
struct json_pointer {
  template<template<class> class>
  friend class basic_json;
};

template struct json_pointer<int>;
template struct json_pointer<char>;
template struct json_pointer<long>;
template struct json_pointer<void>;
