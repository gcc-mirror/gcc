/* This used to ICE. */
/* { dg-do compile } */

struct a {};

class foo : public a, a		// { dg-error "duplicate base" }
{ // { dg-error "-:at end of input" "" { target *-*-* } .+1 } 
