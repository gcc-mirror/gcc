// PR c++/81055
// { dg-do compile { target c++11 } }

int** p = new int*[1]{q};  // { dg-error "not declared" }
