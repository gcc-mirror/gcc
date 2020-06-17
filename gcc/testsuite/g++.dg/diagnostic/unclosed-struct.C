struct unclosed { /* { dg-message "17: to match this '.'" } */
  int dummy;
  // { dg-error "-:expected" "" { target *-*-* } .+1 }
