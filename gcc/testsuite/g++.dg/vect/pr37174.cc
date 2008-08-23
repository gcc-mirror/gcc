/* { dg-do compile } */

int* getFoo();
struct Bar {
        Bar();
        int* foo1;
        int* foo2;
        int* table[4][4][4];
};
Bar::Bar() {
        foo1 = getFoo();
        foo2 = getFoo();
        for (int a = 0; a < 4; ++a) {
                for (int b = 0; b < 4; ++b) {
                        for (int c = 0; c < 4; ++c) {
                                table[a][b][c] = foo1;
                        }
                }
        }
}

/* { dg-final { cleanup-tree-dump "vect" } } */

