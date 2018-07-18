/* Verify that offsetof complains if given a non-standard-layout class.  */
/* Copyright (C) 2003 Free Software Foundation, Inc. */
/* Contributed by Matt Austern <austern@apple.com> 15 May 2003 */

struct X
{
  int x, y;
protected:
  int z;
};

typedef X* pX;
typedef __SIZE_TYPE__ size_t;

size_t yoff = __builtin_offsetof (X, y); /* { dg-message "35:non-standard-layout" } */
