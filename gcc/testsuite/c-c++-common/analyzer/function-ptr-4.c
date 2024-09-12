/* { dg-skip-if "requires hosted libstdc++ for stdlib free" { ! hostedlib } } */

/* Test to see if the analyzer detect and analyze calls via 
   function pointers or not.  */

#include <stdlib.h>

void fun(int *int_ptr)
{
	free(int_ptr); /* { dg-warning "double-'free' of 'int_ptr'" } */
}

void single_call()
{
	int *int_ptr = (int*)malloc(sizeof(int));
	void (*fun_ptr)(int *) = &fun;
	(*fun_ptr)(int_ptr);
}

void double_call()
{
	int *int_ptr = (int*)malloc(sizeof(int));
	void (*fun_ptr)(int *) = &fun;
	(*fun_ptr)(int_ptr); /* { dg-message "calling 'fun' from 'double_call'" } */
	(*fun_ptr)(int_ptr);
}
