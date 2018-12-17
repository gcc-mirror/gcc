/* { dg-do compile } */
/* { dg-options "-Os -w" } */
 
static int aRead() __attribute__((always_inline));
static int aRead() {
    unsigned char h,l;
    l = (*(volatile unsigned char *)(0x78)) ;
    h = (*(volatile unsigned char *)(0x79)) ;
    return (h<<8) | l;
}
 
int main() {
    volatile unsigned char x;
     x = aRead()^42;
 }
 /* { dg-final { scan-assembler "lds r\\d+,121" } } */
