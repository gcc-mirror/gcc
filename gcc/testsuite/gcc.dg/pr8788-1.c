/* PR inline-asm/8788 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef struct {
    long x[6];
} myjmp_buf;

typedef struct {
    myjmp_buf regs;
} my_stack;

void switch_to_stack (my_stack *stack){
    asm (  /* { dg-error "impossible constraint" } */
/* { dg-warning "'asm' operand 1" "asm operand 1" { target *-*-* } .-1 } */
        "\n"
        : "+r" (stack->regs)
    );
}

