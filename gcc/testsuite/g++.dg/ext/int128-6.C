// { dg-do compile { target int128 } }

__int128 i __attribute__((unused));  // { dg-error "1:ISO C\\+\\+ does not support" }

unsigned __int128 ui __attribute__((unused));  // { dg-error "10:ISO C\\+\\+ does not support" }
