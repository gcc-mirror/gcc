// Build don't link:
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 4 Oct 2000 <nathan@codesourcery.com>
// Origin: Bug 543 Gerald Pfeifer <pfeifer@dbai.tuwien.ac.at>

// Bug 532. We failed to bail out when tsubsting a _DECL failed

class ATOMSET
{
};

template <class T>
void addConstsTo(const T &container)
{
typename T::const_iterator l = 0; // ERROR - no type const_iterator
}

void tallyConstants()
{
addConstsTo(ATOMSET());
}
