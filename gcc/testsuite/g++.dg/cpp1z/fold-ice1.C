// PR c++/67926
// { dg-options -std=c++17 }

template <bool ... T> bool FUR = (T && ...);
template <bool ... T> bool FUL = (... && T);

template <bool T1, bool ... T2> bool FBR = (T1 && ... && T2);
template <bool T1, bool ... T2> bool FBL = (T2 && ... && T1);
