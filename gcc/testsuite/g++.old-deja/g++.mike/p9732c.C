// Build don't link:
// prms-id: 9732

struct foo {};
foo& x() { return foo(); }	// WARNING - 
