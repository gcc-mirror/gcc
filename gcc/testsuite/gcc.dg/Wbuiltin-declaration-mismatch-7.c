/* PR c/86125 - missing -Wbuiltin-declaration-mismatch on a mismatched
   return type
   Verify that a declaration of vfprintf() with withe the wrong last
   argument triggers -Wbuiltin-declaration-mismatch even without -Wextra.
   { dg-do compile { target { { lp64 || ilp32 } || llp64 } } }
   { dg-options "-Wbuiltin-declaration-mismatch" } */

struct StdioFile;

typedef __SIZE_TYPE__ size_t;

struct StdioFile;

int fprintf (struct StdioFile*, const char*);   /* { dg-warning "conflicting types for built-in function .fprintf.; expected .int\\\(\[a-z_\]+ \\\*, const char \\\*, \.\.\.\\\)." } */

int vfprintf (struct StdioFile*, const char*, ...);   /* { dg-warning "conflicting types for built-in function .vfprintf.; expected .int\\\(\[a-z_\]+ \\\*, const char \\\*, \[^\n\r,\\\)\]+\\\)." } */

int fputc (char, struct StdioFile*);   /* { dg-warning "conflicting types for built-in function .fputc.; expected .int\\\(int,  void \\\*\\\)." } */

unsigned long long fputs (const char*, struct StdioFile*);   /* { dg-warning "conflicting types for built-in function .fputs.; expected .int\\\(const char \\\*, \[a-z_\]+ \\\*\\\)." } */

int fscanf (struct StdioFile*, const char*, size_t, ...);   /* { dg-warning "conflicting types for built-in function .fscanf.; expected .int\\\(\[a-z_\]+ \\\*, const char \\\*, \.\.\.\\\)." } */

int vfscanf (struct StdioFile*, const char*, ...);   /* { dg-warning "conflicting types for built-in function .vfscanf.; expected .int\\\(\[a-z_\]+ \\\*, const char \\\*, \[^\n\r,\\\)\]+\\\)." } */

size_t fwrite (const void*, size_t, size_t, struct StdioFile);    /* { dg-warning "conflicting types for built-in function .fwrite.; expected .\(long \)?unsigned int\\\(const void \\\*, \(long \)?unsigned int, *\(long \)?unsigned int, *\[a-z_\]+ \\\*\\\)." } */
