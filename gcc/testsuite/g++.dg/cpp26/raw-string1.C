// C++26 P2558R2 - Add @, $, and ` to the basic character set
// { dg-do compile { target c++26 } }

const char *s0 = R"`@$$@`@`$()`@$$@`@`$";
