// Build don't link:
// crash test - XFAIL *-*-*

// by Paul Burchard <burchard@pobox.com>, Level Set Systems, Inc.
// Copyright (C) 1999 Free Software Foundation

class Q {
	template<class>
	class X {
	};
};
template<template<class> class>
class Y {
};
Q::template X<int> x; // ERROR - template syntax

