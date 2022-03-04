module imports.test22714b;
import imports.test22714a;
struct Array(T)
{
    T[] data;
    T[1] smallarray;
}
struct Ensure
{
    Statement ensure;
    Array!Ensure* arraySyntaxCopy;
}
