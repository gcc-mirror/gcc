// { dg-do assemble  }
// Bug: g++ doesn't deal well with abstract declarators used inappropriately.

void (*)();			// { dg-error "" } 
