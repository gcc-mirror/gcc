// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 20 Oct 2004 <nathan@codesourcery.com>

// PR 18095: ICE
// Origin: Volker Reichelt <reichelt@gcc.gnu.org>

struct A {} // { dg-error "expected" }
