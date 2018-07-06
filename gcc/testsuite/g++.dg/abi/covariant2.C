// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 12 Dec 2003 <nathan@codesourcery.com>
// Origin: grigory@stl.sarov.ru

// PR c++/12881. ICE in thunk generation

struct c1 {};

struct c3 : virtual c1
{
    virtual c1* f6() { return 0; }
    int i;
};

struct c6 : virtual c3 { };

struct c7 : c3
{
    virtual c3* f6() { return 0; }
};

struct c24 : virtual c7
{
    virtual c6* f6();
};

c6* c24::f6() {  return 0; }

struct c31 : c24 {};

