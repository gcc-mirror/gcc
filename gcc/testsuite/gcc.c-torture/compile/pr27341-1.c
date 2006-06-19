extern double R_NaReal;
void z_atan2 (double _Complex * r, double _Complex * ccs)
{
    if (*ccs == 0)
        __imag__ *r = R_NaReal;
}

