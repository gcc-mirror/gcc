// Build don't link: 
// GROUPS passed arm
int a, b;

// declaring an array of references should be illegal
int & v[ 2] = { a, b};// ERROR - .*
