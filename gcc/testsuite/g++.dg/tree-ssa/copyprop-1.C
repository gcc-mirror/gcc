/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-dce3" } */

/* Verify that we can eliminate the useless conversions to/from
   const qualified pointer types
     this_2 = o_1;
     D.20003_4 = this_2->data_m;
     this_5 = D.20003_4;
     D.20005_6 = this_5->value;
   copyprop should propagate o_1 and D.20003_4 to the loads of data_m
   and value.  dce removes all traces of this.  */

struct Data {
  int get() const { return value; }
  int value;
};

struct Object {
  int operator[](int i) const { return data_m->get(); }
  Data *data_m;
};

int foo(Object&o)
{
  return o[0];
}

/* Remaining should be two loads.  */

/* { dg-final { scan-tree-dump-times " = \[^\n\]*;" 2 "dce3" } } */
