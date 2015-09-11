/* { dg-do compile } */
/* { dg-options "-fdump-tree-ealias-details -O2" } */

typedef struct
{
  int data16;
}
list_data;
void
fn1 (list_data * p1)
{
    p1->data16 = p1->data16 & 1 & p1->data16 >> 1;
}

