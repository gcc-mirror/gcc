// { dg-do assemble }

// by Paul Burchard <burchard@pobox.com>, Level Set Systems, Inc.
// Copyright (C) 1999 Free Software Foundation

struct Q {
	template<class>
	class X {
	};
	template<template<class> class XX = X> // { dg-bogus "" } (original definition appeared here)
	class Y {
	}; // { dg-bogus "" } redefinition of default argument for `template <class> XX'
	Y<> y;
};

