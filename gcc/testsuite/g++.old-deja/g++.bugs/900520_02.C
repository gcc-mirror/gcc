// { dg-do assemble  }
// g++ 1.37.1 bug 900520_02

// keywords: reference types, initialization, parameter passing

typedef int b_array[3];
typedef int u_array[];

typedef b_array &b_array_ref;
typedef u_array &u_array_ref;

void take_b_array_ref (b_array_ref arg) { } // { dg-error "" } passed to here

extern u_array u_array_gbl_obj;

u_array_ref u_array_ref_gbl_obj0 = u_array_gbl_obj;

b_array_ref b_array_ref_gbl_obj0 = u_array_ref_gbl_obj0; // { dg-error "" } invalid declaration

void test_passing ()
{
  take_b_array_ref (u_array_ref_gbl_obj0); // { dg-error "" } invalid call
}

b_array u_array_gbl_obj;

int main () { return 0; }
