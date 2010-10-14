/* { dg-options "-std=c89" } */
void crash() {
    double l[4];
    if((l[0]+l[2]) && (l[1]+l[3])){
    }
}
