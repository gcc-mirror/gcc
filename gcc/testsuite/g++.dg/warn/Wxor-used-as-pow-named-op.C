/* Verify that -Wxor-used-as-pow only warns with ^ and not with
   named "xor".  */

int t2_16 = 2^16; /* { dg-warning "result of '2\\^16' is 18; did you mean '1 << 16' \\(65536\\)\\?" } */
int t2x16 = 2 xor 16;

int t10_6 = 10^6; /* { dg-warning "result of '10\\^6' is 12; did you mean '1e6'\\?" } */
int t10x6 = 10 xor 6;
