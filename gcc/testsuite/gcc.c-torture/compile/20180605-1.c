void f (int *p, int n)
{
    int j = 0, k;
    
    for (int i = 0; i < n; i++)
        if ((k = *p++) > 0)
            j += k;
    return j;
}
