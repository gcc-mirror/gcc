/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vrp -std=c++11 -fno-strict-aliasing -fdump-tree-dom1" } */

#include <stdio.h>
struct Field {
 virtual int Compare(void*, void*);
};
extern int NKF, NR;
extern int idxs[];
extern Field* the_field;
extern int *incs;
extern char** fptrs;
inline int doCmp(int this_row_offset, int field_idx) {
 void *p = fptrs[field_idx] + this_row_offset * incs[field_idx];
 return the_field->Compare(p,0);
}
bool  Test(void) {

 int row_offset = 0;

 for (; row_offset < NR; ++row_offset) {

   bool is_different = false;
   for (int j = 0; j < NKF ; ++j) {
     int field_idx = idxs[j];
     int cmp = doCmp(row_offset, field_idx);
     fprintf (stderr, "cmp=%d\n",cmp);

     if (cmp == 0) {
       continue;
     }
     if (cmp > 0) {
       is_different = true;
       break;
     } else {
       fprintf (stderr, "Incorrect\n");
       return false;
     }
   }
   if (!is_different) {

     return false;
   }
 }

 return true;
}

// The block ending with cmp == 0 should not be threaded.  ie,
// there should be a single == 0 comparison in the dump file.

// { dg-final { scan-tree-dump-times "== 0" 1 "dom1" } }
// { dg-final { cleanup-tree-dump "dom1" } }
