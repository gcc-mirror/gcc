// Copyright (C) 2003 Free Software Foundation
// Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>
// { dg-options "-Wno-class-conversion" }

struct A {
   operator A&();
};

