// PR c++/71711
// { dg-do compile { target c++17 } }

template < int > struct A {};
template < int ... N > void unary_left (A < (... + N) >);
template < int ... N > void unary_right (A < (N + ...) >);
template < int ... N > void binary_left (A < (42 + ... + N) >);
template < int ... N > void binary_right (A < (N + ... + 42) >);

void bar ()  
{
  // { dg-final { scan-assembler "_Z10unary_leftIJLi1ELi2ELi3EEEv1AIXflplT_EE" } }
  unary_left < 1, 2, 3 > ({});
  // { dg-final { scan-assembler "_Z11unary_rightIJLi1ELi2ELi3EEEv1AIXfrplT_EE" } }
  unary_right < 1, 2, 3 > ({});
  // { dg-final { scan-assembler "_Z11binary_leftIJLi1ELi2ELi3EEEv1AIXfLplLi42ET_EE" } }
  binary_left < 1, 2, 3 > ({});
  // { dg-final { scan-assembler "_Z12binary_rightIJLi1ELi2ELi3EEEv1AIXfRplT_Li42EEE" } }
  binary_right < 1, 2, 3 > ({});
}
