// { dg-do assemble  }
// { dg-options "-Wconversion" }
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 5 May 2001 <nathan@codesourcery.com>

// Bug 2726. We ICE'd trying to say something about possibly confusing
// conversion overload resolution.

class foo
{
};

template<class T>
class bar
{
public:
    operator const T&() const ;
    operator T&() ;

};


template<class T, class Ref, class NodePtr, class ListPtr>
class iterator_template
{
public:
    iterator_template();
    Ref operator*() const;

};

struct IdlDeclarator
{
};

typedef bar< IdlDeclarator > IdlDeclarator_bar;
int
yyparse()

{

  iterator_template<IdlDeclarator_bar,IdlDeclarator_bar&,foo*,foo*> declIter;
  const IdlDeclarator& declarator = *declIter;
  return 1;
}
