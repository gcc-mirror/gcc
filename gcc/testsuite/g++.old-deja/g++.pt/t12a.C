// Build don't link: 

int a (void * x) { return 1; }
typedef void *T;
int b (T x) { return 2; }
