// PR c++/70563
// { dg-do compile { target c++11 } }

template<typename... T> using void_t = void;

template<typename T> struct TemporaryBindObject
{
};

struct MyTrueType
{
 static constexpr bool value = true;
};

struct MyFalseType
{
 static constexpr bool value = false;
};

template<template<typename...> class Dest> struct TestValidBind
{
 template<typename T, typename = void_t<>> struct toTypesOf : MyFalseType
 {};
 template<template<typename...> class Src, typename... Ts> struct toTypesOf<Src<Ts...>, void_t<Dest<Ts...,float>>> : MyTrueType
 {};
};

template<typename T> struct OneParamStruct
{
};
template<typename T1, typename T2> struct TwoParamStruct
{
};

using tmp = TemporaryBindObject<int>;

int main()
{
 bool value1 = TestValidBind<TwoParamStruct>::toTypesOf<TemporaryBindObject<int>>::value;
 bool value2 = TestValidBind<OneParamStruct>::toTypesOf<TemporaryBindObject<int>>::value;
}
