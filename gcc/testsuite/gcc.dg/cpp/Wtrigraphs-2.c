/* { dg-do preprocess } */
/* { dg-options "-std=c99 -Wtrigraphs -fno-show-column" } */

/* Test warnings for trigraphs in comments, with trigraphs enabled.
   Neil Booth.  4 May 2003.  */

/* { dg-bogus "converted" } Test ??< ??= a few ??/ random things in
   { dg-warning "converted" } some ??/
   { dg-bogus "converted" } ??< comments.  */

// { dg-bogus "converted" } More ??/ comment ??> tests.

// { dg-warning "converted" } Another ??/
   Test

// { dg-warning "converted" } And another with space after ??/  
   the escape

// { dg-bogus "converted" } A tricky one ??/\

