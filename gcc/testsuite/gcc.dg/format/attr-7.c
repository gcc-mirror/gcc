/* Test for format attributes: test applying them to types.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat" } */

#include "format.h"

__attribute__((format(printf, 1, 2))) void (*tformatprintf0) (const char *, ...);
void (*tformatprintf1) (const char *, ...) __attribute__((format(printf, 1, 2)));
void (__attribute__((format(printf, 1, 2))) *tformatprintf2) (const char *, ...);
void (__attribute__((format(printf, 1, 2))) ****tformatprintf3) (const char *, ...);

char * (__attribute__((format_arg(1))) *tformat_arg) (const char *);

void
baz (int i)
{
  (*tformatprintf0) ("%d", i);
  (*tformatprintf0) ((*tformat_arg) ("%d"), i);
  (*tformatprintf0) ("%"); /* { dg-warning "format" "prefix" } */
  (*tformatprintf0) ((*tformat_arg) ("%")); /* { dg-warning "format" "prefix" } */
  (*tformatprintf1) ("%d", i);
  (*tformatprintf1) ((*tformat_arg) ("%d"), i);
  (*tformatprintf1) ("%"); /* { dg-warning "format" "postfix" } */
  (*tformatprintf1) ((*tformat_arg) ("%")); /* { dg-warning "format" "postfix" } */
  (*tformatprintf2) ("%d", i);
  (*tformatprintf2) ((*tformat_arg) ("%d"), i);
  (*tformatprintf2) ("%"); /* { dg-warning "format" "nested" } */
  (*tformatprintf2) ((*tformat_arg) ("%")); /* { dg-warning "format" "nested" } */
  (****tformatprintf3) ("%d", i);
  (****tformatprintf3) ((*tformat_arg) ("%d"), i);
  (****tformatprintf3) ("%"); /* { dg-warning "format" "nested 2" } */
  (****tformatprintf3) ((*tformat_arg) ("%")); /* { dg-warning "format" "nested 2" } */
}
