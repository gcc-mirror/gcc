/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

#include <stdlib.h>

int a;

void foo()
{
  a = 123;						  /* count(1) */
}

#include <iostream>
using namespace std;
class Test {
public:
	Test(void){
	cout<< "In Test ctor" << endl;			  /* count(1) */
	}
	~Test(void){
	cout<< "In Test dtor" << endl;			  /* count(1) */
	}
}T1;

void uncalled(void){
	cout<< "In uncalled" << endl;			  /* count(#####) */
}
int main(void){
atexit (&foo);
// Test T2;
cout<< "In main" << endl;				  /* count(1) */
return 0;
}

#include <stdio.h>

__attribute__((constructor))
static void construct_navigationBarImages() {
  fprintf (stderr,  "((construct_navigationBarImages))"); /* count(1) */
}

__attribute__((destructor))
static void destroy_navigationBarImages() {
  fprintf (stderr,  "((destroy_navigationBarImages))");	  /* count(1) */
}

/* { dg-final { run-gcov branches { -b pr16855.C } } } */
