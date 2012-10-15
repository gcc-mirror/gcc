// PR c/37171
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fdump-tree-optimized" }

unsigned int f1 [[gnu::const]] ();
unsigned int f2 [[gnu::__const]] () ;
unsigned int f3 [[gnu::__const__]] () ;

unsigned int f4 ()
{
  return f1 () + f1 () + f1 () + f1 ()
	 + f2 () + f2 () + f2 () + f2 ()
	 + f3 () + f3 () + f3 () + f3 ();
}

// { dg-final { scan-tree-dump-times "= f1 \\(\\)" 1 "optimized" } }
// { dg-final { scan-tree-dump-times "= f2 \\(\\)" 1 "optimized" } }
// { dg-final { scan-tree-dump-times "= f3 \\(\\)" 1 "optimized" } }
// { dg-final { cleanup-tree-dump "optimized" } }
