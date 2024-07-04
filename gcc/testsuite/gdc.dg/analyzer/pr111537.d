// { dg-do compile }
import core.stdc.string;
void main()
{
    char[5] arr;
    strcpy(arr.ptr, "hello world"); // { dg-warning "stack-based buffer overflow" }
}
