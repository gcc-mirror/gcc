/* { dg-do preprocess } */
/* { dg-options "-Wtrigraphs -fno-show-column" } */

/* Test we don't double warn for trigraphs immediately after preceding
   text.  Source Neil Booth.  22 Nov 2000.  */

abcdef??<			/* { dg-warning "ignored" } */
123456??>			/* { dg-warning "ignored" } */
+??=				/* { dg-warning "ignored" } */
