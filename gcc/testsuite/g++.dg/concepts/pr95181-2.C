// { dg-do compile { target concepts } }

template<bool B> struct g {
  g() requires B && false;
  g() requires B;
};

g<true> b; // error
