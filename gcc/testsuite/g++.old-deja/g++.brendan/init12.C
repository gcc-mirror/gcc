// Build don't link: 
// GROUPS passed array-bindings
char * bob();

int main()
{
    char a[1][2];
    a[0] = bob();// ERROR - .*
}
