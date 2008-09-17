// { dg-do assemble  }
// { dg-options "-Wall" }

typedef char * charptr;
typedef __SIZE_TYPE__ size_t;
char c[]={'A','B','C','D'};
int i=size_t(&c);
int *pp=&i;
void foo() { }
int main()
{
 charptr(*pp)++;	// { dg-error "lvalue" } 
 return 0;
}
