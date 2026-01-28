// PR c++/122785
// { dg-do compile { target c++26 } }
// { dg-additional-options "-fmodules -freflection" }

import "reflect-1_a.H";
using ::S;
