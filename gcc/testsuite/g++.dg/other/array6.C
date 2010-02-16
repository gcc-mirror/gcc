// PR c++/43036

typedef char T6[2][8];
const T6* p1;
typedef char T[8];
typedef T T2[2];
typedef T T3[2];
typedef char T5[2][8];
const T2* p2;
const T5* p3;
const T3* p4;
