// GROUPS passed old-abort
// Build don't link: 
int fn();// ERROR -  ambiguates.*
int x;
int& fn() {// ERROR -  new decl.*
return x;}
