// PR c++/50810
// { dg-options "-std=gnu++98 -Wc++0x-compat" }

signed char data[] = { 0xff }; // { dg-warning "narrowing" }
