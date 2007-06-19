extern int v1; // { dg-error "declared" }
static union { int v1; }; // { dg-error "redeclaration" } 

static union { int v2; }; // { dg-error "declared" }
extern int v2; // { dg-error "redeclaration" } 

int v3; // { dg-error "declared" }
static union { int v3; }; // { dg-error "redeclaration" } 

static union { int v4; }; // { dg-error "declared" }
static union { int v4; }; // { dg-error "redeclaration" } 
