// Build don't link:
// Special g++ Options:

volatile void abort();		// WARNING - mismatch
volatile void oink() {
  abort() ;
}				// gets bogus error - 
