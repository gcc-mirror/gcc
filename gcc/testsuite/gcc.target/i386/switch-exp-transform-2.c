/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-switchconv -mbmi" } */

/* Checks that when exponential index transform is viable but switch conversion
   decides that the switch cannot be converted, the exponential index transform
   is not done.  */

int a;

int foo(unsigned bar)
{
    switch (bar)
    {
        case (1 << 0):
            return 0;
        case (1 << 1):
            return 1;
        case (1 << 2):
            return 2;
        case (1 << 3):
            return 3;
        case (1 << 4):
            return 4;
        case (1 << 5):
            a = 3;
            return 5;
        case (1 << 6):
            return 6;
        default:
            return 0;
    }
}

/* { dg-final { scan-tree-dump "Exponential index transform viable" "switchconv" } } */
/* { dg-final { scan-tree-dump-not "Applying exponential index transform" "switchconv" } } */
