// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 1 June 2005 <nathan@codesourcery.com>

// PR 20350: ICE on member specialization with later initialization
// Origin: Carlo Wood carlo@gcc.gnu.org

template <int i> struct Mutex
{
  static int mutex;
};

template <int i>
int Mutex<i>::mutex = {1};

template <> int Mutex<0>::mutex;
template <> int Mutex<0>::mutex = 0;

void g()
{
  Mutex<0>::mutex = 0;
}

