// PR c++/91506

double
test(int *arr, int x)
{
    double ret(double(arr[x]) + 1);
    return ret;
}
