// Build don't link:
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 8 May 2001 <nathan@codesourcery.com>

// Bug 2744. We ICE'd on strange characters

@ // ERROR - parse error
int a; #// ERROR - parse error
## // ERROR - parse error
$ // ERROR - parse error
£ // ERROR - parse error
` // ERROR - parse error
