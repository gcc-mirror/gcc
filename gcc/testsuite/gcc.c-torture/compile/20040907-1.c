void ProdWord_bla ( gtL, gtRes, lnL )
    int *gtL, *gtRes;
    int lnL;
{
    while ( 1 < lnL )
    {
        *gtRes++ = *gtL++;
        --lnL;
    }
    if ( 0 < lnL )
        if ( gtL[0] == gtL[1] )
            *gtRes++ = 0;
}
