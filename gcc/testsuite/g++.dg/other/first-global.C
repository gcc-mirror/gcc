/* { dg-do compile } */
/* { dg-add-options bind_pic_locally } */
/* { dg-final { scan-assembler "_GLOBAL__(I|sub_I)(_|_65535_0_)foobar" } } */

struct foo { foo (); };
foo foobar;
