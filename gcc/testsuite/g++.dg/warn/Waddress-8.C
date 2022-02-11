/* PR c++/96507 - missing -Waddress for member references
   { dg-do compile }
   { dg-options "-Wall" } */

typedef void F ();

extern F &efr;
extern int &eir;

bool warn_ext_rfun ()
{
  return &efr != 0;           // { dg-warning "-Waddress" }
}

bool warn_ext_rvar ()
{
  return &eir != 0;           // { dg-warning "-Waddress" }
}


bool warn_parm_rfun (F &rf)
{
  return &rf != 0;            // { dg-warning "-Waddress" }
}

bool warn_parm_rvar (int &ir)
{
  return &ir != 0;            // { dg-warning "-Waddress" }
}

// Comparing the address of a reference argument to null also triggers
// a -Wnonnull-compare (that seems like a bug, hence PR 103363).
// { dg-prune-output "-Wnonnull-compare" }


struct S
{
  F &fr;
  int &ir;
};

extern S es, esa[];

bool warn_ext_mem_rfun ()
{
  return &es.fr != 0;         // { dg-warning "-Waddress" }
}

bool warn_ext_mem_rvar ()
{
  return &es.ir != 0;         // { dg-warning "-Waddress" }
}


bool warn_ext_arr_mem_rfun (int i)
{
  return &esa[i].fr != 0;     // { dg-warning "-Waddress" }
}

bool warn_ext_arr_mem_rvar (int i)
{
  return &esa[i].ir != 0;     // { dg-warning "-Waddress" }
}


bool warn_parm_mem_rfun (S &s)
{
  return &s.fr != 0;          // { dg-warning "-Waddress" }
}

bool warn_parm_mem_rvar (S &s)
{
  return &s.ir != 0;          // { dg-warning "-Waddress" }
}


bool warn_parm_arr_mem_rfun (S sa[], int i)
{
  return &sa[i].fr != 0;      // { dg-warning "-Waddress" }
}

bool warn_parm_arr_mem_rvar (S sa[], int i)
{
  return &sa[i].ir != 0;      // { dg-warning "-Waddress" }
}
