// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 10 Jan 2001 <nathan@codesourcery.com>

// Bug 1585. We ICEd on a template template parm with no parms.

template<template<class> class C> class B;
template<template<> class C> class D; // { dg-error "" } parse error
