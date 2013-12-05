// { dg-do compile }

char * d [10];  // { dg-message "8: previous declaration as" }
char e [15][10];
int (*f)();

int d;  // { dg-error "" }
