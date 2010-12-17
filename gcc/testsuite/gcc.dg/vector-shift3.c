/* { dg-do compile } */

#define vector(elcount, type)  \
__attribute__((vector_size((elcount)*sizeof(type)))) type


int main (int argc, char *argv[]) {
    vector(8, short) v0 = {argc,2,3,4,5,6,7};
    short sc;

    
    scalar1 <<= v0; /* { dg-error ".*scalar1.*undeclared" } */
   
    return 0;
}

