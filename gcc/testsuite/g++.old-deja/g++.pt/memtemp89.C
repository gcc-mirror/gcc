// Build don't link:

// by Paul Burchard <burchard@pobox.com>, Level Set Systems, Inc.
// Copyright (C) 1999 Free Software Foundation

class Q {
	template<class T>
	class X {
	};
};
template<template<class> class XX>
class Y {
	XX<int> x_;		// ERROR - Q::X not a template
};
Y<Q::X> y;			// ERROR - instantiated from here

