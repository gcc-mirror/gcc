// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 19 Apr 2003 <nathan@codesourcery.com>

// PR 10405. ICE

#define MEM_ENUM(name) int name; enum name {};

struct Base
{
  MEM_ENUM (a)
  MEM_ENUM (b)
  MEM_ENUM (c)
  MEM_ENUM (d)
  MEM_ENUM (e)
  MEM_ENUM (f)
  MEM_ENUM (g)
  MEM_ENUM (h)
  MEM_ENUM (i)
  MEM_ENUM (j)
  MEM_ENUM (k)
  MEM_ENUM (l)
  MEM_ENUM (m)
  MEM_ENUM (n)
  MEM_ENUM (o)
  MEM_ENUM (p)
  MEM_ENUM (q)
  MEM_ENUM (r)
  MEM_ENUM (s)
  MEM_ENUM (t)
  MEM_ENUM (u)
  MEM_ENUM (v)
  MEM_ENUM (w)
    };

struct D : Base  {};

