// PR c++/58632

template<template<int I> class A> // { dg-message "shadows" }
class A {};			// { dg-error "declaration" }
