// Build don't link:

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 11 Jan 2001 <nathan@codesourcery.com>

// Bug 1506. We ICE'd on a struct definition inside a template parms.
// This is still not completely fixed, but now issues a diagnostic

// crash test - XFAIL *-*-*

template<class T =
struct W {};    // ERROR - inside template parms
