// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 16 Jun 2005 <nathan@codesourcery.com>

// Crash when compiler is optimized
// Origin:  Andrew Pinski pinskia@gcc.gnu.org

struct facet { virtual ~facet(); };
struct ctype_base {};
struct ctype : facet, ctype_base {};
