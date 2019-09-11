/* { dg-require-effective-target int32plus } */
/* { dg-require-effective-target size32plus } */
/* { dg-require-stack-size "128*1024" } */

void crc()
{
    int  toread;
    long long nleft;
    unsigned char buf[(128 * 1024)];

    nleft = 0;
    while (toread = (nleft < (2147483647 * 2U + 1U)) ? nleft: (2147483647 * 2U + 1U) )
	;
}
