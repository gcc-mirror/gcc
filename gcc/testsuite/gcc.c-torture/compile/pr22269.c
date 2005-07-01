/* We used to ICE in tree-ssa-reassoc because we did look at the correct operand to
   see if it was a SSA_NAME.  */
int printf(const char*, ...);
int main(int argv, char*argc) {

    int d1;
    int d2;
    int s1, s2;
    int b;
    ((d1)&=(int)0x0000ffffL, (d1)|=((int)(short)(0x344))<<16);
    ((d1)&=(int)0xffff0000UL, (d1)|=(int)(unsigned short)(0x4567));
    ((d2)&=(int)0x0000ffffL, (d2)|=((int)(short)(0))<<16);
    ((d2)&=(int)0xffff0000UL, (d2)|=(int)(unsigned short)(0x3b9a));
    printf(" dividend >>: %ld\n", d1);
    printf(" divisor  >>: %ld\n", d2);
}
