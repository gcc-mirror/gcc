/* { dg-options "-O2 -fdump-tree-optimized" } */

void f( int x );

void h( unsigned ix )
{
  switch( ix )
  {
    case 0: f(42); break;
    case 1: f(42); break;
    case 2: f(42); break;
    case 3: f(42); break;
    case 4: f(42); break;
    case 5: f(42); break;
    default: __builtin_unreachable();
  }
}


/* { dg-final { scan-tree-dump-not "if" "optimized" } } */
/* { dg-final { scan-tree-dump-not "switch" "optimized" } } */
