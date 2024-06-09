/* Test C23 UTF-8 characters.  Test errors for invalid code.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

unsigned char a = u8''; /* { dg-error "empty character constant" } */
unsigned char b = u8'ab'; /* { dg-error "multi-character literal cannot have an encoding prefix" } */
unsigned char c = u8'\u00ff'; /* { dg-error "character not encodable in a single code unit" } */
unsigned char d = u8'\x100'; /* { dg-error "hex escape sequence out of range" } */
