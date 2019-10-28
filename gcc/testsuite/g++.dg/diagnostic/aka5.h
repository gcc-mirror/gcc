#ifdef IS_SYSTEM_HEADER
#pragma GCC system_header
#endif

typedef enum __internal_enum { A, B } user_enum;
typedef user_enum *user_enum_ptr;

typedef struct __internal_struct { int i; } user_struct;
typedef user_struct user_struct_copy;
typedef user_struct *user_struct_ptr;

typedef union __internal_union { int i; } user_union;
typedef user_union user_union_copy;
typedef user_union *user_union_ptr;

typedef unsigned int user_vector __attribute__((__vector_size__(16)));
typedef user_vector user_vector_copy;
typedef user_vector *user_vector_ptr;

typedef int user_int;
typedef user_int user_int_copy;
typedef user_int *user_int_ptr;
