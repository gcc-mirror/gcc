// GROUPS passed niklas hiding ARM
// Build don't link:
// Special g++ Options: -w
struct stat {};
stat gstat;
int stat (struct stat*);
void f () { struct stat* ps; stat (ps); }
