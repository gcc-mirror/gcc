// Build don't link:

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 10 Jan 2001 <nathan@codesourcery.com>
// Origin snyder@fnal.gov

// Bug 933. Secondary vtables weren't correctly located for non-primary
// virtual bases. Causing us to ICE.

class d0_Object
{
public:
virtual ~d0_Object ();
};


class M10 : virtual public d0_Object {};
class M4 : virtual public M10 {};

class M9
: public M4, virtual public M10
{
public:
M9 ();
};

M9::M9 () {}
