/* PR c/86125 - missing -Wbuiltin-declaration-mismatch on a mismatched
   return type
   Verify that declarations of file I/O built-ins with an arbitrary
   object pointer do not trigger -Wbuiltin-declaration-mismatch.
   { dg-do compile }
   { dg-options "-Wbuiltin-declaration-mismatch -Wextra" } */

typedef __SIZE_TYPE__ size_t;

struct StdioFile;

int fprintf (struct StdioFile*, const char*, ...);
int vfprintf (struct StdioFile*, const char*, __builtin_va_list);
int fputc (int, struct StdioFile*);
int fputs (const char*, struct StdioFile*);
int fscanf (struct StdioFile*, const char*, ...);
int vfscanf (struct StdioFile*, const char*, __builtin_va_list);
size_t fwrite (const void*, size_t, size_t, struct StdioFile*);
