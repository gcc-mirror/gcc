// PR driver/115440
// { dg-do compile { target c++17_only } }
// { dg-options "--c++17" }

int i;

// { dg-bogus "unrecognized command-line option '--c\\\+\\\+17'; did you mean '--stdc\\\+\\\+17'" "" { target *-*-* } 0 }
// { dg-error "unrecognized command-line option '--c\\\+\\\+17'" "" { target *-*-* } 0 }
