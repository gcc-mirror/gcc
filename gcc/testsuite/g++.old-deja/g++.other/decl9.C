// { dg-do assemble  }
// Build don't link
// Origin: batali@cogsci.ucsd.edu
// Contributed by Gabriel Dos Reis <gdr@codesourcery.com>

typedef struct { } S;           // OK
typedef struct { };             // { dg-error "1:missing type-name" } Missing type-name

typedef union { } U;            // OK
typedef union { };              // { dg-error "1:missing type-name" } Missing type-name
