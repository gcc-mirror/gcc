// { dg-do assemble  }
// prms-id: 6149

int a[3 - sizeof(double)];	// { dg-error "" } 
