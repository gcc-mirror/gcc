/* PR120353: Test for -Wflex-array-member-not-at-end on structure with 
   typedef.  */ 
/* { dg-do compile } */
/* { dg-options "-Wflex-array-member-not-at-end" } */

typedef struct flex flex_t;
struct flex { int n; int data[]; };
struct out_flex_mid {flex_t flex_data;  int m; }; /* { dg-warning "structure containing a flexible array member is not at the end of another structure" } */

typedef struct flex flex_t1;
struct out_flex_mid1 {flex_t1 flex_data1; int n; }; /* { dg-warning "structure containing a flexible array member is not at the end of another structure" } */ 
