module imports.b313;

void bug()
{
    // scope has access to it's own module
    imports.b313.bug();
}
