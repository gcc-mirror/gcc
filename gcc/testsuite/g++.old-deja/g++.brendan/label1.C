// Build don't link: 
// GROUPS passed labels
// it should only give 1 error, about using an undefined label
int main(void) { goto dummy; }// ERROR - .*
