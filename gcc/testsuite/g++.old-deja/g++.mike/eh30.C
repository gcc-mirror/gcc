// { dg-do assemble }
// { dg-options "-fexceptions -fPIC -S" }
// { dg-require-effective-target fpic }


int
main() { throw 1; }
