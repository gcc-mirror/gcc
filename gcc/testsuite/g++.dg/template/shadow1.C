// PR c++/58632

template<template<int I> class A> // { dg-message "template parameter" }
class A {};			// { dg-error "shadows" }
