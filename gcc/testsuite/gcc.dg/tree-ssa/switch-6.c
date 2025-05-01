/* { dg-do compile { target { { x86_64-*-* aarch64-*-* ia64-*-* powerpc64-*-* } && lp64 } } } */
/* { dg-options "-O2 -fdump-tree-switchlower1 -fno-jump-tables" } */

/* Test that bit-test switch lowering can create cluster of size 64 (there was
   an of-by-one error causing it to only do 63 before).  */

int f();    

int foo(int a)
{
    switch (a)
    {
        case 0:
        case 3:
        case 5:
        case 7:
        case 9:
        case 11:
        case 13:
        case 15:
        case 17:
        case 19:
        case 21:
        case 23:
        case 25:
        case 27:
        case 29:
        case 31:
        case 33:
        case 35:
        case 37:
        case 39:
        case 41:
        case 43:
        case 45:
        case 47:
        case 49:
        case 51:
        case 53:
        case 55:
        case 57:
        case 59:
        case 61:
        case 63:
            return f();
        default:
            return -1;
    }
}

/* { dg-final { scan-tree-dump ";; GIMPLE switch case clusters: BT:0-63" "switchlower1" } } */
