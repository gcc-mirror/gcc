// { dg-do compile }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 5 Sept 2001 <nathan@codesourcery.com>

// Bug 3986. Another indirect primary base problem.

struct Consts
{
};

struct MathLib :
  virtual Consts
{
};

struct Parallel :
  virtual Consts
{
};

struct Particles :
  virtual MathLib,
  virtual Parallel
{
};

struct Ring :
  virtual Particles
{
};

struct Injection :
  virtual Particles,
  virtual Ring
{
};

struct LSpaceCharge :
  virtual Ring,
  virtual Injection
{
};

struct Bump :
  virtual Consts
{
};

struct Output :
  virtual Injection,
  virtual Bump
{
};

struct Plots :
  virtual LSpaceCharge,
  virtual Output
{
};
