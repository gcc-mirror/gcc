/* { dg-options "-mthumb -Os -fpic -fdump-rtl-hoist" }  */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-require-effective-target fpic } */
/* Make sure all calculations of gObj's address get hoisted to one location.  */
/* { dg-final { scan-rtl-dump "PRE/HOIST: end of bb .* copying expression" "hoist" } } */

struct st_a {
   int data;
};

struct st_b {
   struct st_a *p_a;
    struct st_b *next;
};

extern struct st_b gObj;
extern void foo(int, struct st_b*);

int goo(struct st_b * obj) {
   struct st_a *pa;
   if (gObj.p_a->data != 0) {
     foo(gObj.p_a->data, obj);
   }
   pa = obj->p_a;
   if (pa == 0) {
     return 0;
   } else if (pa == gObj.p_a) {
     return 0;
   }
   return pa->data;
}
