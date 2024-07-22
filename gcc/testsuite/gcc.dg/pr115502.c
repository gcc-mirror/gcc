/* PR c/115502 */
/* { dg-do compile { target lto } } */
/* { dg-options "-std=c23 -flto" } */

typedef struct _OSet OSet;
typedef OSet AvlTree;
void vgPlain_OSetGen_Lookup(const OSet *);
struct _OSet {};
void vgPlain_OSetGen_Lookup(const AvlTree *);
