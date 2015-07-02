// PR c++/53690
// { dg-do compile { target c++11 } }

int array1[U'\U00000000' == 0 ? 1 : -1];
int array2[U'\u0000' == 0 ? 1 : -1];
int array3[u'\U00000000' == 0 ? 1 : -1];
int array4[u'\u0000' == 0 ? 1 : -1];
