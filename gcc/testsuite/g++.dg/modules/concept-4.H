// { dg-additional-options "-std=c++2a -fmodule-header" }
// { dg-module-cmi {} }

template<typename T>
inline constexpr bool disable = false;

template<typename U>
concept sized = true;

template<typename V>
class TPL
{
};

template<typename W>
requires (!sized<W>)
inline constexpr bool disable<TPL<W>>
= true;
