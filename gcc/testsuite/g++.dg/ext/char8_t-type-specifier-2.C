// Test that char8_t is recognized as a type specifier if -fchar8_t is enabled.
// { dg-do compile }
// { dg-options "-fchar8_t" }

char8_t c8;

signed char8_t         sc8;            /* { dg-error "signed" } */
unsigned char8_t       uc8;            /* { dg-error "unsigned" } */

short char8_t          shc8;           /* { dg-error "short" } */
long char8_t           lgc8;           /* { dg-error "long" } */

signed short char8_t   ssc8;           /* { dg-error "signed" } */
signed long char8_t    slc8;           /* { dg-error "signed" } */
unsigned short char8_t usc8;           /* { dg-error "unsigned" } */
unsigned long char8_t  ulc8;           /* { dg-error "unsigned" } */
