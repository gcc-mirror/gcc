/* { dg-do compile } */

__extension__ typedef __UINTPTR_TYPE__ uintptr_t;

typedef struct TypHeader {
     struct TypHeader * * ptr;
     unsigned char type;
 } * TypHandle;
 extern TypHandle (* EvTab[81]) ( TypHandle hd );
 TypHandle FunApplyRel ( TypHandle hdCall )
 {
     TypHandle hdApp;
     TypHandle * ptApp;
     long lp;
     long lc;
     hdApp = ((uintptr_t)(((TypHandle*)((hdCall)->ptr))[1])&1 ?
(((TypHandle*)((hdCall)->ptr))[1]) : (*
EvTab[(((uintptr_t)(((TypHandle*)((hdCall)->ptr))[1]) & 1) ? 1 :
((((TypHandle*)((hdCall)->ptr))[1])->type))])((((TypHandle*)((hdCall)->ptr))[1])));
     ptApp = ((TypHandle*)((hdApp)->ptr));
     ptApp[1] = ((TypHandle) (uintptr_t) (((long)(lp) << 2) + 1));
     ptApp[2] = ((TypHandle) (uintptr_t) (((long)(lc) << 2) + 1));
 }

/* { dg-final { cleanup-tree-dump "vect" } } */
