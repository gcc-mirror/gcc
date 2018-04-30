/* PR target/80569 */
/* { dg-do assemble } */
/* { dg-options "-O2 -m16 -march=haswell" } */

void load_kernel(void *setup_addr)
{
    unsigned int seg = (unsigned int)setup_addr >> 4;
    asm("movl %0, %%es" : : "r"(seg));
}
