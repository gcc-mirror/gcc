// Build don't link:
// Special g++ Options:

namespace std {
volatile void abort();		// WARNING - mismatch
}

volatile void oink() {
  std::abort() ;
}				// gets bogus error - 
