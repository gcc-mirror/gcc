/* { dg-options "-fdiagnostics-show-caret -std=c++11" } */

const void *s = u8"a"  u"b";  // { dg-error "24: concatenation" }
/* { dg-begin-multiline-output "" }
 const void *s = u8"a"  u"b";
                 ~~~~~  ^~~~
   { dg-end-multiline-output "" } */

const void *s2 = u"a"  u"b"  u8"c";  // { dg-error "30: concatenation" }
/* { dg-begin-multiline-output "" }
 const void *s2 = u"a"  u"b"  u8"c";
                        ~~~~  ^~~~~
  { dg-end-multiline-output "" } */

#define TEST_U8_LITERAL u8"a"

const void *s3 = TEST_U8_LITERAL u8"b";

const void *s4 = TEST_U8_LITERAL u"b"; // { dg-error "34: concatenation" }
/* { dg-begin-multiline-output "" }
 const void *s4 = TEST_U8_LITERAL u"b";
                                  ^~~~
  { dg-end-multiline-output "" } */
