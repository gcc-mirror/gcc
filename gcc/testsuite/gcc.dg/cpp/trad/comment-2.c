/* Test for warning of nested comments.  */

/* { dg-do preprocess } */

/* { dg-options "-traditional-cpp -Wcomments" }

/* /* */   /* { dg-warning "within comment" } */

/*

 /* { dg-warning "within comment" } */
