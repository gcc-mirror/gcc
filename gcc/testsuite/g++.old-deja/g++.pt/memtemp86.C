// { dg-do assemble { xfail *-*-* } }

// by Paul Burchard <burchard@pobox.com>, Level Set Systems, Inc.
// Copyright (C) 1999 Free Software Foundation

struct Q {
	template<class>
	class X {
	};
};
template<template<class> class>
class Y {
};
Y<typename Q::X> y; // { dg-error "" } typename out of template context

