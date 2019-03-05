// REQUIRED_ARGS: -w

void main()
{
    int i;
    switch (i)
    {
        case 1, 2:
        case 3, 4: break;
        default: break;
    }

    char ch;
    switch (ch)
    {
        case 'U', 'u':
        case 'L', 'l':
        default:
    }

    switch (i)
    {
        default: case 1:
        case 3,4:
    }
}
