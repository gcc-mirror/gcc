// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 12 Oct 2005 <nathan@codesourcery.com>

// PR 21117:ICE after error
// Origin: Andrew Pinski <pinskia@gcc.gnu.org>

struct wxString;
struct wxString* wxGetEmptyString();

struct wxString GetHeader() // { dg-error "return type" }
{
  return *wxGetEmptyString();
}


