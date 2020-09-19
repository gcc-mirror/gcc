/* PR c/50584 - No warning for passing small array to C99 static array
   declarator
   { dg-do compile }
   { dg-options "-Wall" } */

// Reduced from Glibc.

typedef struct FILE FILE;

int vfprintf (FILE*, const char*, __builtin_va_list);
int vfprintf (FILE*, const char*, __builtin_va_list);   // { dg-bogus "-Warray-parameter" }
int vfprintf (FILE*, const char*, __builtin_va_list);

int vfscanf (FILE*, const char*, __builtin_va_list);
int vfscanf (FILE*, const char*, __builtin_va_list);    // { dg-bogus "-Warray-parameter" }
int vfscanf (FILE*, const char*, __builtin_va_list);


/* Verify that mismatches in array/to pointer to va_list are still
   diagnosed.  */

int fva (__builtin_va_list);
int fva (__builtin_va_list);

int fpva_a1 (__builtin_va_list*);
int fpva_a1 (__builtin_va_list[1]);     // { dg-warning "\\\[-Warray-parameter" }

int fpva_a_ (__builtin_va_list*);
int fpva_a_ (__builtin_va_list[]);
int fpva_a_ (__builtin_va_list*);
int fpva_a_ (__builtin_va_list[]);

/* Also verify that a mismatch between a pointer and a one-element
   array are diagnosed.  This is pervasive in Glibc headers but
   making an exception for it would leave no way to express
   the requirement that a function take at least one argument
   by reference.  */

struct __jmp_buf_tag;
int __sigsetjmp (struct __jmp_buf_tag*, int);

struct __jmp_buf_tag { };
typedef struct __jmp_buf_tag jmp_buf[1];

int __sigsetjmp (struct __jmp_buf_tag[1], int);   // { dg-warning "\\\[-Warray-parameter" }
