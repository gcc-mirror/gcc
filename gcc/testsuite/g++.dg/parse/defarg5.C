// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 5 Sep 2003 <nathan@codesourcery.com>
// Origin:Wolfgang Bangerth bangerth@dealii.org

// PR c++/12167 - infinite recursion

typedef int int32_t __attribute__((mode (__SI__)));
typedef unsigned uint32_t __attribute__((mode (__SI__)));

class A {   
  void report(int32_t d
	      // the default arg is what NAN etc can expand to, but
	      // with the floatiness removed.
	      = (__extension__ ((union { uint32_t l; int32_t d; })
				{ l: 0x7fc00000U }).d));
};
