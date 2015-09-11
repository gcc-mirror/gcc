/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt1" } */
/* { dg-add-options bind_pic_locally } */

struct my_array {
    int data[4];
};

const int& my_max(const int& a, const int& b) {
    return a < b ? b : a;
}

int f(my_array a, my_array b) {
    int res = 0;
    for (int i = 0; i < 4; ++i) {
	res += my_max(a.data[i], b.data[i]);
    }
    return res;
}

/* { dg-final { scan-tree-dump "MAX_EXPR" "phiopt1" } } */
