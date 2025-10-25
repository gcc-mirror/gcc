/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

unsigned char reg(_Bool b) {
    union U {
        unsigned char f0;
        _Bool f1;
    };
    union U u;
    u.f1 = b;
    if (u.f0 > 1) {
        // This cannot happen
        // if b is only allowed
        // to be 0 or 1:
        return 42;
    }
    return 13;
}

/* { dg-final { scan-tree-dump "return 13"  "evrp" } } */
