namespace unclosed { /* { dg-message "20: to match this '.'" } */
int filler;
/* { dg-error "-:expected '.' at end of input" "" { target *-*-* } .+1 } */
