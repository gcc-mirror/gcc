// Build don't link: 
// Special g++ Options: -Wshadow
// GROUPS passed niklas scoping ARM
class X { X (int); };
void X (int);// ERROR - .*hides constructor.*
void f () { X (1); }
