// { dg-do assemble  }
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 8 May 2001 <nathan@codesourcery.com>

// Bug 2744. We ICE'd on strange characters

@ // { dg-error "" } parse error
int a; #// { dg-error "" } parse error
## // { dg-error "" } parse error
$ // { dg-error "" } parse error
£ // { dg-error "" } parse error
` // { dg-error "" } parse error
