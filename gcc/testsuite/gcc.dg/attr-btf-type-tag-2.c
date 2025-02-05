/* Test btf_type_tag attribute argument checking for wide string types.  */
/* { dg-do compile } */
/* { dg-options "--std=c11" } */

int __attribute__((btf_type_tag (U"Ustr"))) x; /* { dg-error "unsupported wide string" } */

int __attribute__((btf_type_tag (u"ustr"))) y; /* { dg-error "unsupported wide string" } */

int __attribute__((btf_type_tag (u8"u8str"))) z; /* OK.  */
