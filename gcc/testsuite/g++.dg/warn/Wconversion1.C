// { dg-options "-fsigned-char -Wconversion" }

char c1 = 1024; // { dg-warning "overflow" }
char c2 = char(1024);
char c3 = (char) 1024;
char c4 = static_cast<char>(1024);
 
unsigned char uc1 = -129; // { dg-warning "unsigned" }

bool b1 = -3;

int i1 = 0x80000000;
