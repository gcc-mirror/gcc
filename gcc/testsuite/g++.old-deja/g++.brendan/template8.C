// { dg-do assemble  }
// GROUPS passed templates
#include <stdio.h>

// make sure we accept unions for templates
template<int n>
union Double_alignt{
	double for_alignt;
	char array[n];

};

int main(){

	
	Double_alignt<20000> heap;

	printf(" &heap.array[0] = %d, &heap.for_alignt = %d\n", &heap.array[0], &heap.for_alignt);

}
