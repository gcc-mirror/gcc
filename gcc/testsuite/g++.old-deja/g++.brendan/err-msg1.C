// Build don't link: 
// GROUPS passed error-messages
class A { };

int i = A::_ter;// ERROR -  ._ter.*
int j = A::term;// ERROR -  .term.*
