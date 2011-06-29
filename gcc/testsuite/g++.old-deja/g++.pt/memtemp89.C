// { dg-do assemble  }

// by Paul Burchard <burchard@pobox.com>, Level Set Systems, Inc.
// Copyright (C) 1999, 2000 Free Software Foundation

class Q {
	template<class T>
	class X {		// { dg-error "" } Q::X private
	};
};
template<template<class> class XX>
class Y {
	XX<int> x_;
};
Y<Q::X> y;			// { dg-error "" } required from here
