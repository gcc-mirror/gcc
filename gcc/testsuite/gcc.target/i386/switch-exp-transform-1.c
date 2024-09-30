/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-switchconv -fdump-tree-widening_mul -mpopcnt -mbmi" } */

/* Checks that exponential index transform enables switch conversion to convert
   this switch into an array lookup.  Also checks that the "index variable is a
   power of two" check has been generated and that it has been later expanded
   into an internal function.  */

int foo(unsigned bar)
{
    switch (bar)
    {
        case (1 << 0):
            return 1;
        case (1 << 1):
            return 2;
        case (1 << 2):
            return 3;
        case (1 << 3):
            return 4;
        case (1 << 4):
            return 8;
        case (1 << 5):
            return 13;
        case (1 << 6):
            return 21;
        default:
            return 0;
    }
}

/* { dg-final { scan-tree-dump "CSWTCH" "switchconv" } } */
/* { dg-final { scan-tree-dump "POPCOUNT" "widening_mul" } } */
