/* { dg-do preprocess } */
/* { dg-options "-std=c99 -Wtrigraphs -fno-show-column" } */

/* Test we don't double warn for trigraphs immediately after preceding
   text.  Source Neil Booth.  4 May 2003.  */

/* { dg-bogus "ignored" } Test ??< ??= a few ??/ random things in
   { dg-warning "converted" } some ??/
   { dg-bogus "ignored" } ??< comments.  */

// { dg-bogus "ignored" } More ??/ comment ??> tests.

// { dg-warning "converted" } Another ??/
   Test

// { dg-warning "converted" } And another with space after ??/  
   the escape

// { dg-bogus "ignored" } A tricky one ??/\

