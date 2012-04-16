// { dg-options "" }
// PR c++/24052

struct A { };
int main() { b: A() && && b; } // { dg-error "operand types are 'A' and 'void\\*'" }
// { dg-message "candidate|operator&&|no known conversion" "additional" { target *-*-* } 5 }
