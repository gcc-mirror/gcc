// Build don't link: 
// GROUPS passed array-bindings
char * bob();

main()
{
    char a[1][2];
    a[0] = bob();// ERROR - .*
}
