// Build don't link: 
// GROUPS passed friends
// do_friend should complain that foo was declared as a friend of
// A before A was defined
struct A;
struct B { friend A::foo (); };// ERROR - .*
