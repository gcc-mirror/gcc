// { dg-do assemble  }
// { dg-options "-fexceptions" }

int main() { try { 1; } }	// { dg-error "" } 
