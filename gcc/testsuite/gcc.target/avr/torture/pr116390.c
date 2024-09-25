/* { dg-do compile } */
/* { dg-additional-options "-std=c99" } */

typedef struct
{
    int i;
} MyStruct;

void f_ic (int, char);
void f_pi (char const *, int);
void f_iic (int, int, char);

const MyStruct *f_rms (void);

char *f_rcp (void);
int f_ri (void);

void badFoo (void)
{
    const MyStruct* ps = f_rms ();
    const char* pc = f_rcp ();

    unsigned n1 = f_rcp () - pc;

    if (n1)
    {
        long n2 = n1 - ps->i;
        if (n2 > 0)
        {
            if (f_ri ())
                n2 = n1;

            if (f_ri ())
            {
                f_iic (1, 2 * n2, ' ');
            }
            else
                f_pi (pc, n2);
        }
        if (ps->i > 0)
        {
            if (n2 >= 0)
                f_pi (pc + n2, ps->i);
            else
            {
                f_ic (n2, ' ');
            }
        }

        const int which = f_ri ();
        switch (which)
        {
        case 1:
            if (f_ri ())
                f_rcp ()[1] = ' ';
            break;

        case 2:
            f_pi (f_rcp (), 1);
            break;

        case 3:
            if (f_ri () && n1 < 0)
                f_ic (n1, ' ');
            else
                f_rcp ()[1] = ' ';
            break;

        }
    }
}
