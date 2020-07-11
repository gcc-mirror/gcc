// PR c++/94733
// { dg-do compile { target c++11 } }
// { dg-options "-fgnu-tm" }

struct [[gnu::may_alias]] pe { };
