/* This checks for two things:
   - an obscure corner case in the standard rules for __LINE__
   - regression of an associated bug in cpplib where the semicolon got lost */
/* { dg-do compile } */

enum { i = __LINE__\
};

char array[i == 6 ? 1 : -1];
