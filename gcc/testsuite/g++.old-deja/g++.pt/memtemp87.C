// { dg-do assemble }

// by Paul Burchard <burchard@pobox.com>, Level Set Systems, Inc.
// Copyright (C) 1999, 2002 Free Software Foundation

class Q {
public:
	template<class>
	class X {
	};
};
template<template<class> class>
class Y {
};
Q::template X<int> x; // { dg-error "template" "" { target c++98 } }
