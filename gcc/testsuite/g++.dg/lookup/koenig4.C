// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 5 Oct 2004 <nathan@codesourcery.com>

// Origin: Wolfgang Bangerth <bangerth@dealii.org>
// Incorrect koenig lookup

struct A {}; 
 
struct B { 
    static void foo(); 
    static void bar(const A &); 
};   
     
void bar(const A &){} 
     
void B::foo () {    
    A a; 
    bar (a); 
}
