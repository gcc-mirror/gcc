// Build don't link
// Origin: batali@cogsci.ucsd.edu
// Contributed by Gabriel Dos Reis <gdr@codesourcery.com>

typedef struct { } S;           // OK
typedef struct { };             // ERROR - Missing type-name

typedef union { } U;            // OK
typedef union { };              // ERROR - Missing type-name
