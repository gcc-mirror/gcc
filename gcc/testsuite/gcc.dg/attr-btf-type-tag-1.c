/* Test btf_type_tag attribute argument checking.  */
/* { dg-do compile } */

void * __attribute__((btf_type_tag ("A"), btf_type_tag ("vptr"))) a;

int __attribute__((btf_type_tag (5))) b; /* { dg-error "requires a string" } */

char * __attribute__((btf_type_tag (L"Lstr"))) c; /* { dg-error "unsupported wide string" } */

int * __attribute__((btf_type_tag)) d; /* { dg-error "wrong number of arguments" } */

char * __attribute__((btf_type_tag ("A", "B"))) e; /* { dg-error "wrong number of arguments" } */
