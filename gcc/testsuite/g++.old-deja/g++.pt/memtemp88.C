// Build don't link:
// crash test - XFAIL *-*-*

// by Paul Burchard <burchard@pobox.com>, Level Set Systems, Inc.
// Copyright (C) 1999 Free Software Foundation

struct Q {
	template<class>
	class X {
	};
	template<template<class> class XX = X> // gets bogus error - (original definition appeared here)
	class Y {
	}; // gets bogus error - redefinition of default argument for `template <class> XX'
	Y<> y;
};

