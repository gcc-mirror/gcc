// Build don't link:
// Special g++ Options: -Wall

typedef char * charptr;
char c[]={'A','B','C','D'};
int i=int(&c);
int *pp=&i;
void foo() { }
int main()
{
 charptr(*pp)++;	// WARNING - 
 return 0;
}
