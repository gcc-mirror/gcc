// { dg-do compile }
// { dg-options "-O -fdump-tree-cddce1" }

enum Scale  { E1, E2, E3, E4, E5, E6, E7, E8 };

int Test(Scale s)
{ 
  switch(s)
    {
      case E1: return 12;
      case E2: return 17;
      case E3: return 22;
      case E4: return 42;
      default:  break;
    }
  return 0;
}

// tree forwprop should have eliminated the (int) s cast for the
// switch value and directly switch on the 's' parameter

// { dg-final { scan-tree-dump-not "\\\(int\\\)" "cddce1" } }
// { dg-final { scan-tree-dump "switch \\\(s_.\\\(D\\\)\\\)" "cddce1" } }
// { dg-final { cleanup-tree-dump "cddce1" } }
