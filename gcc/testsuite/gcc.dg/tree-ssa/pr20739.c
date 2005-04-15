/* PR middle-end/20739 */

/* dg-do compile */
/* dg-options "-O" */

/* We used to fail to compile this because gimplification dropped the
   conversion that added the const qualifier to the sub-expression
   involving baz, and then immediately noticed and reported its
   absence.  */

typedef struct 
{ 
    char chars[5]; 
} 
baz_t; 
 
extern baz_t * baz; 
 
extern void foo (baz_t); 
int 
bar (const baz_t * ls) 
{ 
    foo (ls == 0 ? *(&baz[0]) : *ls); 
}
