/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Test operator ## semantics.  */

#define bad1 ## owt	/* { dg-error "cannot" "## at objlike start" } */
#define bad2 owt ##	/* { dg-error "cannot" "## at objlike end" } */
#define bad3(x) ## x	/* { dg-error "cannot" "## at funlike start" } */
#define bad4(x) x ##	/* { dg-error "cannot" "## at funlike end" } */
