// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 5 Sep 2003 <nathan@codesourcery.com>
// Origin:Wolfgang Bangerth bangerth@dealii.org

// PR c++/12167 - infinite recursion

class A {   
  void report(int d
	      // the default arg is what NAN etc can expand to, but
	      // with the floatiness removed.
	      = (__extension__ ((union { unsigned l; int d; })
				{ l: 0x7fc00000U }).d));
};
