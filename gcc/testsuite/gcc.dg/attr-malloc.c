/* PR middle-end/94527 - Add an attribute that marks a function as freeing
   an object
   Verify that attribute malloc with one or two arguments is accepted where
   intended and rejected where it's invalid.
   { dg-options "-Wall -ftrack-macro-expansion=0" } */

#define A(...) __attribute__ ((malloc (__VA_ARGS__)))

A (0) void* alloc_zero (int);           // { dg-error "'malloc' attribute argument 1 does not name a function" }

A ("") void* alloc_string (int);        // { dg-error "'malloc' attribute argument 1 does not name a function" }

int var;
A (var) void* alloc_var (int);          // { dg-error "'malloc' attribute argument 1 does not name a function" }

typedef struct Type { int i; } Type;
A (Type) void* alloc_type (int);        // { dg-error "expected expression|identifier" }

A (unknown) void* alloc_unknown (int);  // { dg-error "'unknown' undeclared" }

void fv_ ();                            // { dg-message "declared here" }
A (fv_) void* alloc_fv_ (int);          // { dg-error "'malloc' attribute argument 1 must take a pointer type as its first argument" }

void fvi (int);                         // { dg-message "declared here" }
A (fvi) void* alloc_fvi (int);          // { dg-error "'malloc' attribute argument 1 must take a pointer type as its first argument; have 'int'" }

void fvv (void);                        // { dg-message "declared here" }
A (fvv) void* alloc_fvv (int);          // { dg-error "'malloc' attribute argument 1 must take a pointer type as its first argument; have 'void'" }

void fvi_ (int, ...);                   // { dg-message "declared here" }
A (fvi_) void* alloc_fvi_ (int);        // { dg-error "'malloc' attribute argument 1 must take a pointer type as its first argument; have 'int'" }

void fvi_vp (Type, void*);              // { dg-message "declared here" }
A (fvi_vp) void* alloc_fvi_vp (int);    // { dg-error "'malloc' attribute argument 1 must take a pointer type as its first argument; have 'Type'" }


void fpv (void*);
A (fpv) void* alloc_fpv (int);

void fpv_i (void*, int);
A (fpv_i) void* alloc_fpv_i (int);

void fpv_pv (void*, void*);
A (fpv_i) void* alloc_fpv_pv (int);


void gpc (char*);
void hpi (int*);
A (fpv) A (gpc) A (hpi) Type* alloc_fpv_gpv (int);


/* Verify that the attribute can be applied to <stdio.h> functions.  */
typedef struct FILE FILE;
typedef __SIZE_TYPE__ size_t;

int   fclose (FILE*);
FILE* fdopen (int);
FILE* fopen (const char*, const char*);
FILE* freopen (const char*, const char*, FILE*);
int   pclose (FILE*);
FILE* popen (const char*, const char*);
FILE* tmpfile (void);

A (fclose) A (freopen, 3) A (pclose)
  FILE* fdopen (int);
A (fclose) A (freopen, 3) A (pclose)
  FILE* fopen (const char*, const char*);
A (fclose) A (freopen, 3) A (pclose)
  FILE* fmemopen(void *, size_t, const char *);
A (fclose) A (freopen, 3) A (pclose)
  FILE* freopen (const char*, const char*, FILE*);
A (fclose) A (freopen, 3) A (pclose)
  FILE* popen (const char*, const char*);
A (fclose) A (freopen, 3) A (pclose)
  FILE* tmpfile (void);
