typedef struct { int i; } T1;
typedef T1 T2;
extern T1 a;
extern T2 b;
int main() { return a.i + b.i; }
