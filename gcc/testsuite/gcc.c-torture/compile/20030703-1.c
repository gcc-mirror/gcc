/* Extracted from PR target/10700.  */
/* The following code used to cause an ICE on 64-bit targets.  */

int SAD_Block(int *);
void MBMotionEstimation(int *act_block, int block)
{
    SAD_Block(act_block + (  (8 * (block == 1 || block == 3))
                          + (8 * (block == 2 || block == 3))));
}

