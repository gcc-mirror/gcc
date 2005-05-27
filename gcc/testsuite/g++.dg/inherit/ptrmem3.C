// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 27 May 2005 <nathan@codesourcery.com>

// Origin:Andrew Pinski pinskia@gcc.gnu.org
// PR 21455  bogus error with pointer to member of incomplete

class XMLFile;

typedef bool (XMLFile::*ParserFunctionPtr)();

struct ParserElement
{
  ParserFunctionPtr getPreFunc() const { return preFunc; }
  ParserFunctionPtr getPostFunc() const { return postFunc; }
  ParserFunctionPtr preFunc;
  ParserFunctionPtr postFunc;
};
