// { dg-do compile }
// { dg-options "-Wstrict-aliasing -fstrict-aliasing" }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 29 Sep 2002 <nathan@codesourcery.com>

// 8083. warn about odd casts

typedef int YYSTYPE;
typedef struct tDefEntry 
{
  unsigned t;
  
} tDefEntry;
struct incomplete;


YYSTYPE
 addSibMacro(
         YYSTYPE  list )
 {
     tDefEntry** ppT   = (tDefEntry**)&list; // { dg-warning "type-punned pointer" "" }
 
     struct incomplete *p = (struct incomplete *)&list; // { dg-warning "type-punning to incomplete" "" }
     
     return list;
 }

