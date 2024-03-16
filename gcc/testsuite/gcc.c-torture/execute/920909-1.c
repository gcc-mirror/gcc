/* { dg-additional-options "-std=gnu89" } */
f(a){switch(a){case 0x402:return a+1;case 0x403:return a+2;case 0x404:return a+3;case 0x405:return a+4;case 0x406:return 1;case 0x407:return 4;}return 0;}
main(){if(f(1))abort();exit(0);}
