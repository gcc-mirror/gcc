// Build don't link: 
// GROUPS passed old-abort
int main()
{
    int a[100], **p;

    p = &a[50];// ERROR -  assignment to.*

}
