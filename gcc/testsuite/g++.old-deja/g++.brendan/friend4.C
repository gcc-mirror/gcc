// { dg-do assemble  }
// GROUPS passed friends
// do_friend should complain that foo was declared as a friend of
// A before A was defined
struct A; // { dg-message "forward" } 
struct B { friend A::foo (); };// { dg-error "" } .*
