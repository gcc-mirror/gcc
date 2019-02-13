// { dg-do assemble  }
// prms-id: 6149

int a[3 - sizeof(double)];	// { dg-error "9:size .\[0-9\]+. of array .a. exceeds maximum object size" }
