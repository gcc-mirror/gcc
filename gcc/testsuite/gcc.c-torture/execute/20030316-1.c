/* PR target/9164 */
/* The comparison operand was sign extended erraneously.  */

void abort (void);

int
main (void)
{
    long j = 0x40000000;
    if ((unsigned int) (0x40000000 + j) < 0L)
 	abort ();

    return 0;
}
