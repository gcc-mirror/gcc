/* { dg-do compile } */
/* { dg-options "-g" } */

struct S{int x,y[1];}*a;
int main(void){
	struct S{int x,y[];};
}

