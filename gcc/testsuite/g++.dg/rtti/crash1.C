// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 29 Sep 2002 <nathan@codesourcery.com>

// PR 7788. ICE

class foo;
extern const foo bar;
class bar;
