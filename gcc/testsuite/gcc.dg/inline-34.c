/* Diagnostics for bad references to static objects and functions from
   inline definitions must take account of declarations after the
   definition which make it not an inline definition.  PR 39556.  */
/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */

static int a1;
inline int f1 (void) { return a1; }
int f1 (void);

static int a2;
inline int f2 (void) { return a2; }
extern inline int f2 (void);

inline void f3 (void) { static int a3; }
void f3 (void);

inline void f4 (void) { static int a4; }
extern inline void f4 (void);
