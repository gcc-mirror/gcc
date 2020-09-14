// { dg-do compile }
// { dg-options "-Waddress" }

void* ptr;

int test()
{
    if (&ptr) // { dg-warning "the address of 'ptr' will always evaluate as 'true'" }
        return 1;

    return 0;
}
