// Build don't link: 
// Special g++ Options: -pedantic-errors
// GROUPS passed arm
enum color {red, yellow, green=20, blue};
color c = 1;	// this should be an error// ERROR - .*
int i = yellow;
