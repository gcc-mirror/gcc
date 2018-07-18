// PR c++/71946
// { dg-do compile { target c++11 } }
// { dg-options "" }

auto test = []{ int t = ({ int t1; t1 = 7; t1; }); };
