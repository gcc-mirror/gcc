// Build don't link:

// by Paul Burchard <burchard@pobox.com>, Level Set Systems, Inc.
// Copyright (C) 1999, 2000 Free Software Foundation

class Q {
	template<class T>
	class X {		// ERROR - Q::X private
	};
};
template<template<class> class XX>
class Y {
	XX<int> x_;
};
Y<Q::X> y;			// ERROR - instantiated from here
