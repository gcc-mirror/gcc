/* { dg-do preprocess } */
/* { dg-options "-std=gnu99 -Wtrigraphs -fno-show-column" } */

/* Test we don't double warn for trigraphs immediately after preceding
   text.  Source Neil Booth.  22 Nov 2000.  */

abcdef??<			/* { dg-warning "ignored" } */
123456??>			/* { dg-warning "ignored" } */
+??=				/* { dg-warning "ignored" } */

/* Test we warn of escaped newlines only in comments.  Source Neil
   Booth.  4 May 2003.  */

/* { dg-bogus "ignored" } Test ??< ??= a few ??/ random things in
   { dg-warning "ignored" } some ??/
   { dg-bogus "ignored" } ??< comments.  */

// { dg-bogus "ignored" } More ??/ comment ??> tests.

// { dg-warning "ignored" } Another ??/
   Test

// { dg-warning "ignored" } And another with space after ??/  
   the escape

// { dg-bogus "ignored" } A tricky one ??/\

