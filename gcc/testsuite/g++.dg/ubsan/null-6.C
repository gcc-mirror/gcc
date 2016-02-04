// PR c++/67941
// { dg-do run { target c++11 } }
// { dg-options -fsanitize=null }

int main(){ (+[](){})(); }
