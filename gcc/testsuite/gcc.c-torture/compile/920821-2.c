typedef struct{int p[25];}t1;
struct{t1 x,y;}y;
t1 x[1];
void f(void){y.x=*x;y.y=*x;}
