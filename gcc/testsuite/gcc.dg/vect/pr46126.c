/* { dg-do compile } */

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
     hdApp = ((long)(((TypHandle*)((hdCall)->ptr))[1])&1 ?
(((TypHandle*)((hdCall)->ptr))[1]) : (*
EvTab[(((long)(((TypHandle*)((hdCall)->ptr))[1]) & 1) ? 1 :
((((TypHandle*)((hdCall)->ptr))[1])->type))])((((TypHandle*)((hdCall)->ptr))[1])));
     ptApp = ((TypHandle*)((hdApp)->ptr));
     ptApp[1] = ((TypHandle) (((long)(lp) << 2) + 1));
     ptApp[2] = ((TypHandle) (((long)(lc) << 2) + 1));
 }

/* { dg-final { cleanup-tree-dump "vect" } } */
