// { dg-do assemble { target native } }
// { dg-options "-fexceptions -fPIC -S" }

int
main() { throw 1; }
