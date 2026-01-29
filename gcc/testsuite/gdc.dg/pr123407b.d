// { dg-do compile }
module pr123407;
int fun();
extern(C) int _D8pr1234073funFZi; // { dg-error "matches conflicting symbols" }
