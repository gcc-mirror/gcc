/* PR c/86125 - missing -Wbuiltin-declaration-mismatch on a mismatched
   return type
   Verify that declarations of file I/O built-ins with different
   definitions of struct FILE triggers -Wbuiltin-declaration-mismatch
   when -Wextra is specified.
   { dg-do compile }
   { dg-options "-Wall -Wbuiltin-declaration-mismatch" } */

struct FooFile;
int fputc (int, struct FooFile*);

typedef struct FooFile AlsoFooFile;
int fprintf (AlsoFooFile*, const char*, ...);

typedef AlsoFooFile* FooFilePtr;
int fscanf (FooFilePtr, const char*, ...);

/* No warning here (-Wextra not specified).  */
struct BarFile;
int vfprintf (struct BarFile*, const char*, __builtin_va_list);


/* Set -Wextra and verify -Wbuiltin-declaration-mismatch is issued.  */
#pragma GCC diagnostic warning "-Wextra"

int fputs (const char*, struct BarFile*);   /* { dg-warning "mismatch in argument 2 type of built-in function .fputs.; expected .struct FooFile \\\*." } */
