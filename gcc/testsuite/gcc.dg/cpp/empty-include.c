/* 
 * Copyright 2004 Free Software Foundation, Inc.
 * Contributed and written by Nathanael Nerode.
 *
 * GCC 3.4 would attempt to open stdin as the included file
 * (PR 17610), causing a sort of hang.
 * 
 * We should get an error.
 */

/* {dg-do preprocess} */
#include "" /* { dg-error "empty" "error on empty filename in include" } */
int x; /* Otherwise we have an empty file and get more errors. */
