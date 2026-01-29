// { dg-do compile }
module pr123407;
pragma(mangle, "ice") int fun();
pragma(mangle, "ice") int var; // { dg-error "matches conflicting symbols" }
