/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */


int main() 
{
	void func(int a, struct {int _[a];} v) {}	/* { dg-warning "struct" } */
}

