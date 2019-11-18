/* Test C2x UTF-8 characters.  Test errors for invalid code.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

unsigned char a = u8''; /* { dg-error "empty character constant" } */
unsigned char b = u8'ab'; /* { dg-error "character constant too long for its type" } */
unsigned char c = u8'\u00ff'; /* { dg-error "character constant too long for its type" } */
unsigned char d = u8'\x100'; /* { dg-error "hex escape sequence out of range" } */
