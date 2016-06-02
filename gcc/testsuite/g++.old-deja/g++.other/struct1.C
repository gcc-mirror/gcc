// { dg-do assemble  }

// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 3 Jun 1999 <nathan@acm.org>

// Duplicate definitions are wrong, we should just cough
// politely, but we used to die horribly.

class Y // { dg-message "" } previous definition
{
};
class Y // { dg-error "" } redefinition
{   
};

template<class T> class X // { dg-message "" } previous definition
{
};
template<class T> class X // { dg-error "" } redefinition
{   
};

template<class T> class X<T *> // { dg-message "" } previous definition
{
};
template<class T> class X<T *> // { dg-error "" } redefinition
{   
};

template<> class X<int> // { dg-message "" } previous definition
{
};
template<> class X<int> // { dg-error "" } redefinition
{   
};

template<> class X<int *> // { dg-message "" } previous definition
{
};
template<> class X<int *> // { dg-error "" } redefinition
{   
};
