// Build don't link: 
// GROUPS passed miscellaneous
// The compiler shouldn't give a `invalid operands to binary +' for this
// case.
enum flag { OFF, ON };
enum BOOL { FALSE = (enum flag) 0, TRUE };
