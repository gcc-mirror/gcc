// Bug: g++ doesn't deal well with abstract declarators used inappropriately.
// Build don't link:

void (*)();			// ERROR - 
