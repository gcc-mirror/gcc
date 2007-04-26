typedef unsigned long long HARD_REG_ELT_TYPE;
static HARD_REG_ELT_TYPE reload_reg_used_in_output_addr[30];
int reload_reg_reaches_end_p (unsigned int regno, int opnum)
{
    int i;
    for (i = opnum + 1; i < opnum; i++)
        if (reload_reg_used_in_output_addr[i]
            & ((HARD_REG_ELT_TYPE)1 << regno))
            return 0;
}

