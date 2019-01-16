// PERMUTE_ARGS: -inline -g -O -d
void main()
{
    auto darray1 = new int*[](10);
    foreach(ref v; darray1)
        v = new int;
    auto darray2 = new int*[](10);
    darray2[] = darray1[]; // calls memset instead of memcpy
    foreach(i; 0 .. 10)
        assert(darray1[i] == darray2[i]);
}
