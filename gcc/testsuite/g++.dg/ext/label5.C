// { dg-options "" }
// PR c++/24052

struct A { };
int main() { b: A() && && b; } // { dg-error "A\\(\\) && && *b" }

// { dg-message "candidate" "additional" { target *-*-* } 5 }
