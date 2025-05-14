// { dg-do compile }
// { dg-options "-O2 -fdump-tree-evrp" }

int always1(int a, int b) {
    if (a / b)
        return b != 0;
    return 1;
}

// If b != 0 is optimized by recognizing divide by 0 cannot happen,
// there should be no PHI node.

// { dg-final { scan-tree-dump-not "PHI" "evrp" } }
