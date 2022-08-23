int testCppCMangle (unsigned long long val, char ch)
{
    int vch = (char)val;
    if (vch != ch)
        return 0;
    return vch;
}
