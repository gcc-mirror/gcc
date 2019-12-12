struct S
{
    int[] data;
}
immutable X = S([]);
enum len = X.data.length;
