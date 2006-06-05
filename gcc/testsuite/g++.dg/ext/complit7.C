// PR c++/27807
// { dg-options "" }

int i = (int()){0}; // { dg-error "type" }
