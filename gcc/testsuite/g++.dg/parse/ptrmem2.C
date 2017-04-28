// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 2 Dec 2004 <nathan@codesourcery.com>

// PR 18782: ICE with ptr-to-member
// Origin:   Volker Reichelt <reichelt@gcc.gnu.org>

namespace A {}

int A::* p; // { dg-error "is a namespace" }
