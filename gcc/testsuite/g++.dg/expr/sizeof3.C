// PR c++/15337

class CCC; 
int main() { sizeof(CCC); return 0; } // { dg-error "14:invalid application of .sizeof. to incomplete type .CCC." }
