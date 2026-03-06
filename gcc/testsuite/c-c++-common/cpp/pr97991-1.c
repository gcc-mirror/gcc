/* { dg-do compile } */
/* Make sure malformed redefine_extname with a pragma inside does not cause an ICE. */
#pragma redefine_extname _Pragma ("pack(bar)") /* { dg-warning "" } */
