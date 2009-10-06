/* { dg-lto-do link } */
/* { dg-lto-options {{-fstrict-aliasing -flto}} } */

typedef struct { } t_commrec;
typedef struct { } t_fft_c;
void
solve_pme(t_commrec *cr)
{
    t_fft_c *ptr;
} 
int main () { return 0; }
