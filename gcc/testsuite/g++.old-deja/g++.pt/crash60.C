// { dg-do assemble  }
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 7 Nov 2000 <nathan@codesourcery.com>

// We ICE'd rather than fail to instantiate.

template< typename SID, class SDR >
void k( SID sid, SDR* p,
 void (SDR::*)
 ( typename SID::T ) );		// { dg-message "note" }

struct E { };
struct S { void f( int ); };

void f()
{
  k( E(), (S*)0, &S::f );   // { dg-error "" } no match
  // { dg-message "candidate" "candidate note" { target *-*-* } 17 }
} 
