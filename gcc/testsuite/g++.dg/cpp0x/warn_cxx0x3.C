// PR c++/50810
// { dg-do compile { target c++11 } }
// { dg-options "-std=gnu++98 -Wc++11-compat -Wno-narrowing" }

signed char data[] = { 0xff };
