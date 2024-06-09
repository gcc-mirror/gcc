void abort (void);
void exit (int);
long f(short a,short b){return (long)a/b;}
int main(void){if(f(-32768,-1)!=32768L)abort();else exit(0);}
