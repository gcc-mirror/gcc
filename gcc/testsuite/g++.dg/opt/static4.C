// PR 13898
// Make sure the two X variables get assigned unique assembler names
// if they are promoted to static storage.

// { dg-do compile }

int g(int i) { 
  if (i<1) { 
    const int x[3] = { 1,2,3 }; 
    return x[i]; 
  } else { 
    const int x[3] = { 4,5,6 }; 
    return x[i]; 
  } 
} 
