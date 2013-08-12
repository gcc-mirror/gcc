extern int v1; // { dg-message "declared" }
static union { int v1; }; // { dg-error "redeclaration" } 

static union { int v2; }; // { dg-message "declared" }
extern int v2; // { dg-error "redeclaration" } 

int v3; // { dg-message "declared" }
static union { int v3; }; // { dg-error "redeclaration" } 

static union { int v4; }; // { dg-message "declared" }
static union { int v4; }; // { dg-error "redeclaration" } 
