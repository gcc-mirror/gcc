// Build don't link:
// Special g++ Options:

volatile void abort();
volatile void oink() {
  abort() ;
}				// gets bogus error - 
