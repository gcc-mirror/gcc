/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options -dD } */

/* Test -dD does not fail.  */

#define objlike obj like
#define funlike(like) fun like
#define funlike2(fun, like) fun ## like
#define varargs(x, ...) x #x __VA_ARGS__
#define gnu_varargs(x, y...) x ## y 
