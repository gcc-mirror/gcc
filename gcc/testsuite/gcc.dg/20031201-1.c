/* PR optimization/12628 */
/* The following test used to ICE in init_alias_analysis because the
   given command line options meant that reg_scan wasn't (re)run before
   the jump bypassing pass.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fno-expensive-optimizations -fno-rerun-loop-opt" } */

int outbuf[100];
int outcnt;
int bi_buf;
void send_bits(void)
{
    bi_buf = 0;
    outbuf[outcnt++] = 8;
    outbuf[outcnt++] = 8;
    if (outcnt)
        bi_buf = 1;
}

