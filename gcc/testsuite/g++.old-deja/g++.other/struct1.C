// Build don't link:

// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 3 Jun 1999 <nathan@acm.org>

// Duplicate definitions are wrong, we should just cough
// politely, but we used to die horribly.

class Y
{   // ERROR - previous definition
};
class Y
{   // ERROR - redefinition
};

template<class T> class X
{   // ERROR - previous definition
};
template<class T> class X
{   // ERROR - redefinition
};

template<class T> class X<T *>
{   // ERROR - previous definition
};
template<class T> class X<T *>
{   // ERROR - redefinition
};

template<> class X<int>
{   // ERROR - previous definition
};
template<> class X<int>
{   // ERROR - redefinition
};

template<> class X<int *>
{   // ERROR - previous definition
};
template<> class X<int *>
{   // ERROR - redefinition
};
