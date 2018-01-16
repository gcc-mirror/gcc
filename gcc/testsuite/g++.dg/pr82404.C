// PR middle-end/82404

// { dg-do compile }
// { dg-options "-O3 -Wall -fdump-tree-optimized -Wno-return-type" } 

enum eShape { eSquare, eCircle, eShpere, eTetraeder };

double test_switch_native(enum eShape shape, double r) {
    switch(shape) {
    case eSquare:    return 2;
    case eCircle:    return 3;
    case eShpere:    return 4;
    case eTetraeder: return 5;
    }
}

// { dg-final { scan-tree-dump-not "if "  optimized } }
