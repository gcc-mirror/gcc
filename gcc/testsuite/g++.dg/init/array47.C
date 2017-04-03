// PR c++/78572

static int array[10] = { array[3]=5, array[7]=3, };
