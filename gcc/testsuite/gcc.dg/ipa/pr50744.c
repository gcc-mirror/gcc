/* { dg-do compile } */
/* { dg-options "-O3 -fno-optimize-sibling-calls" } */

extern int use_data (void *p_01, void *p_02, void *p_03, void *p_04, void *p_05,
		     void *p_06, void *p_07, void *p_08, void *p_09, void *p_10,
		     void *p_11, void *p_12, void *p_13, void *p_14, void *p_15,
		     void *p_16, void *p_17, void *p_18, void *p_19, void *p_20,
		     void *p_21, void *p_22, void *p_23, void *p_24, void *p_25,
		     void *p_26, void *p_27, void *p_28, void *p_29,
		     void *p_30);

extern int idx (int i, int j, int n);

struct stuff
{
  int decision;
  int *a, *b, *c;
  int res;
};


#define some_large_stuff(stuff, n) { \
  int i, j, k; \
  for (i = 0; i < n; i++) \
    for (j = 0; j < n; j++) \
      { \
	int v = stuff->c[idx(i, j, n)]; \
	for (k = 0; k < n; k++) \
	  v += stuff->a[idx(i, k, n)] * stuff->b[idx(k,j,n)]; \
	stuff->c[idx(i, j, n)] = v; \
      } \
}

#define recursion if (iter > 0) \
    foo (stuff, iter - 1, (void *) -1, p_01, p_02, p_03, p_04, p_05, p_06, \
      p_07, p_08, p_09, p_10, p_11, p_12, p_13, p_14, p_15, p_16, p_17, \
     p_18, p_19, p_20, p_21, p_22, p_23, p_24, p_25, p_26, p_27, p_28, p_29); \
    else \
      foo (stuff, iter, p_01, p_02, p_03, p_04, p_05, p_06, p_07, p_08, p_09, \
	p_10, p_11, p_12, p_13, p_14, p_15, p_16, p_17, p_18, p_19, p_20, \
        p_21,p_22, p_23, p_24, p_25, p_26, p_27, p_28, p_29, p_30)

void
foo (struct stuff *stuff,
     int iter,
     void *p_01, void *p_02, void *p_03, void *p_04, void *p_05,
     void *p_06, void *p_07, void *p_08, void *p_09, void *p_10,
     void *p_11, void *p_12, void *p_13, void *p_14, void *p_15,
     void *p_16, void *p_17, void *p_18, void *p_19, void *p_20,
     void *p_21, void *p_22, void *p_23, void *p_24, void *p_25,
     void *p_26, void *p_27, void *p_28, void *p_29, void *p_30)
{
 switch (stuff->decision)
   {
   case 0:
     some_large_stuff (stuff, 83);
     stuff->res =
       use_data (p_01, p_02, p_03, p_04, p_05, p_06, p_07, p_08, p_09, p_10,
		 p_11, p_12, p_13, p_14, p_15, p_16, p_17, p_18, p_19, p_20,
		 p_21, p_22, p_23, p_24, p_25, p_26, p_27, p_28, p_29, p_30);
     recursion;
     break;

   case 1:
     some_large_stuff (stuff, 25);
     stuff->res =
       use_data (p_11, p_02, p_03, p_04, p_05, p_06, p_07, p_08, p_09, p_10,
		 p_21, p_12, p_13, p_14, p_15, p_16, p_17, p_18, p_19, p_20,
		 p_01, p_22, p_23, p_24, p_25, p_26, p_27, p_28, p_29, p_30);
     recursion;
     break;

   case 3:
     some_large_stuff (stuff, 139);
     stuff->res =
       use_data (p_01, p_12, p_03, p_04, p_05, p_06, p_07, p_08, p_09, p_10,
		 p_11, p_22, p_13, p_14, p_15, p_16, p_17, p_18, p_19, p_20,
		 p_21, p_02, p_23, p_24, p_25, p_26, p_27, p_28, p_29, p_30);
     recursion;
     break;

   case 4:
     some_large_stuff (stuff, 32);
     stuff->res =
       use_data (p_01, p_02, p_13, p_04, p_05, p_06, p_07, p_08, p_09, p_10,
		 p_11, p_12, p_23, p_14, p_15, p_16, p_17, p_18, p_19, p_20,
		 p_21, p_22, p_03, p_24, p_25, p_26, p_27, p_28, p_29, p_30);
     recursion;
     break;

   case 5:
     some_large_stuff (stuff, 205);
     stuff->res =
       use_data (p_01, p_02, p_03, p_04, p_15, p_06, p_07, p_08, p_09, p_10,
		 p_11, p_12, p_13, p_14, p_25, p_16, p_17, p_18, p_19, p_20,
		 p_21, p_22, p_23, p_24, p_05, p_26, p_27, p_28, p_29, p_30);
     recursion;
     break;

   case 6:
     some_large_stuff (stuff, 64);
     stuff->res =
       use_data (p_01, p_02, p_03, p_04, p_05, p_16, p_07, p_08, p_09, p_10,
		 p_11, p_12, p_13, p_14, p_15, p_26, p_17, p_18, p_19, p_20,
		 p_21, p_22, p_23, p_24, p_25, p_06, p_27, p_28, p_29, p_30);
     recursion;
     break;
   }
}

#define NULL (void *)0

void
bar (struct stuff *stuff, int iter)
{
  foo (stuff, iter, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
       NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
       NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
}
