/* PR target/80569 */
/* { dg-do assemble } */
/* { dg-options "-O2 -m16 -march=haswell" } */
/* Non-gas assemblers choke on .code16gcc.  */
/* { dg-require-effective-target gas } */

void load_kernel(void *setup_addr)
{
    unsigned int seg = (unsigned int)setup_addr >> 4;
    asm("movl %0, %%es" : : "r"(seg));
}
