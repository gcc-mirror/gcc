// PR c++/13438

struct A { friend typename struct B; };  // { dg-error "" }
  
