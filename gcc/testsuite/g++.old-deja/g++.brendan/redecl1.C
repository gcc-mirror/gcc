// Build don't link: 
// GROUPS passed redeclaration
// crash test - XFAIL *-*-*
inline int min(int x, int y) {return x < y ? x : y;}	/* 235 */// ERROR - .*
int min(int a, int b);
inline int min(int a, int b) {return (a < b)?a:b;}// ERROR - .*
