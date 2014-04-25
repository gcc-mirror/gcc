/* PR c/18079 */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

__attribute__ ((always_inline)) void fndecl1 (void);
__attribute__ ((noinline)) void fndecl1 (void); /* { dg-warning "attribute 'noinline' follows declaration with attribute 'always_inline'" } */

__attribute__ ((noinline)) void fndecl2 (void);
__attribute__ ((always_inline)) void fndecl2 (void); /* { dg-warning "attribute 'always_inline' follows declaration with attribute 'noinline'" } */


__attribute__ ((hot)) void fndecl3 (void);
__attribute__ ((cold)) void fndecl3 (void); /* { dg-warning "attribute 'cold' follows declaration with attribute 'hot'" } */

__attribute__ ((cold)) void fndecl4 (void);
__attribute__ ((hot)) void fndecl4 (void); /* { dg-warning "attribute 'hot' follows declaration with attribute 'cold'" } */
