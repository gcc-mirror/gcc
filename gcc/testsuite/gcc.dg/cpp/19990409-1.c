/* Test that __LINE__ works when embedded in a macro. */
/* { dg-do compile } */

#define XLINE __LINE__

char array[XLINE == __LINE__ ? 1 : -1];
