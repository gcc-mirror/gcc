/* Test for -Wflex-array-member-not-at-end on structure/union with 
   C99 flexible array members being embedded into another structure.  */
/* { dg-do compile } */
/* { dg-options "-Wflex-array-member-not-at-end" } */

struct flex { int n; int data[]; };
struct out_flex_end { int m; struct flex flex_data; }; /* { dg-bogus "structure containing a flexible array member is not at the end of another structure" } */
struct out_flex_mid { struct flex flex_data; int m; };  /* { dg-warning "structure containing a flexible array member is not at the end of another structure" } */
/* since the warning has been issued for out_flex_mid, no need to
   issue warning again when it is included in another structure/union.  */
struct outer_flex_mid { struct out_flex_mid out_flex_data; int p; }; /* { dg-bogus "structure containing a flexible array member is not at the end of another structure" } */
union flex_union_mid { int a; struct outer_flex_mid b; }; /* { dg-bogus "structure containing a flexible array member is not at the end of another structure" } */


struct flex0 { int n; int data[0]; };
struct out_flex_end0 { int m; struct flex0 flex_data; }; /* { dg-bogus "structure containing a flexible array member is not at the end of another structure" } */
struct out_flex_mid0 { struct flex0 flex_data; int m; };  /* { dg-bogus "structure containing a flexible array member is not at the end of another structure" } */
struct outer_flex_mid0 { struct out_flex_mid0 out_flex_data; int p; }; /* { dg-bogus "structure containing a flexible array member is not at the end of another structure" } */
union flex_union_mid0 { int a; struct outer_flex_mid0 b; }; /* { dg-bogus "structure containing a flexible array member is not at the end of another structure" } */

struct flex1 { int n; int data[1]; };
struct out_flex_end1 { int m; struct flex1 flex_data; }; /* { dg-bogus "structure containing a flexible array member is not at the end of another structure" } */
struct out_flex_mid1 { struct flex1 flex_data; int m; }; /* { dg-bogus "structure containing a flexible array member is not at the end of another structure" } */ 
struct outer_flex_mid1 { struct out_flex_mid1 out_flex_data; int p; }; /* { dg-bogus "structure containing a flexible array member is not at the end of another structure" } */
union flex_union_mid1 { int a; struct outer_flex_mid1 b; }; /* { dg-bogus "structure containing a flexible array member is not at the end of another structure" } */

struct flexn { int n; int data[8]; }; 
struct out_flex_endn { int m; struct flexn flex_data; }; /* { dg-bogus "structure containing a flexible array member is not at the end of another structure" } */
struct out_flex_midn { struct flexn flex_data; int m; }; /* { dg-bogus "structure containing a flexible array member is not at the end of another structure" } */ 
struct outer_flex_midn { struct out_flex_midn out_flex_data; int p; }; /* { dg-bogus "structure containing a flexible array member is not at the end of another structure" } */
union flex_union_midn { int a; struct outer_flex_midn b; }; /* { dg-bogus "structure containing a flexible array member is not at the end of another structure" } */
