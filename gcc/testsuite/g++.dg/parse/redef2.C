// { dg-do compile }

char * d [10];  // { dg-error "8: 'd' has a previous declaration as" }
char e [15][10];
int (*f)();

int d;  // { dg-error "" }
