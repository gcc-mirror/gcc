/* PR c/10175 */

/* { dg-do compile } */
/* { dg-options "-Wunreachable-code" } */

int value;

int main(void)
{
    if (0)
        value = 0;  /* { dg-warning "will never be executed" "" } */
    else
        value = 1;

    return 0;
}

