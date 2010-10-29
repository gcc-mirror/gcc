/* { dg-do compile } */
#define vector(elcount, type)  \
__attribute__((vector_size((elcount)*sizeof(type)))) type

int main (int argc, char *argv[]) {
    vector(4,char) vchar = {1,2,3,4};
    vector(4, int) vint  = {1,1,1,1};
    
    vint <<= vchar;  /* { dg-error "nvalid operands to binary <<" } */
    vchar >>= vint;  /* { dg-error "nvalid operands to binary >>" } */

    return 0;
}

