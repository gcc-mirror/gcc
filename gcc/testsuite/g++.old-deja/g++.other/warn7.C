// Build don't link:
// Special g++ Options: -W -Wall
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 April 2001 <nathan@codesourcery.com>

// Bug 2356. Unused parameter information introduced in a ctor
// definition was not propagated to clones, leading to missed or
// unwarranted unused parameter warnings, possibly given twice.

struct X
{
  X(int i);
  void foo (int i);
  
};
void foo (int i);

X::X(int)
{
}
void X::foo (int) 
{
}
void foo (int)
{
}

struct Y
{
  Y(int);
  void bar (int);
  
};
void bar (int);

Y::Y(int i)
{ // WARNING - unused parameter
}
void Y::bar (int i) 
{ // WARNING - unused parameter
}
void bar (int i)
{ // WARNING - unused parameter
}
