/* { dg-do compile } */
/* { dg-options "" } */

void e(int c)
{
    goto foo;									/* { dg-error "jump into scope" } */
    [[maybe_unused]] struct d { [[gnu::vector_size(4)]] char an[c]; } q;
foo:
}

