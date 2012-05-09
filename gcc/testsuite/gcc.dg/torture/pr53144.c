/* { dg-do compile } */

typedef unsigned char __attribute__((vector_size(4))) uvec;

int main (int argc, char *argv[]) {
    int i;
    int x = 0;
    uvec uc0 = (uvec) {argc, 1,  2,  10};
    unsigned char uc1[4] = {0, 3, 2, 200};
    signed char ucg[4] = {1, 0, 0, 0 };
    signed char ucl[4] = {0, 1, 0, 1 };

#define uc0_ ((unsigned char *)&uc0)

    for (i = 0; i < 4; i ++) {
	x |= ucg[i] != (uc0_[i] > uc1[i]);
	x |= ucl[i] != (uc0_[i] < uc1[i]);
    }
    return x;
}

