// { dg-do assemble  }
// GROUPS passed old-abort
int main()
{
    int a[100], **p;

    p = &a[50];// { dg-error "" }  assignment to.*

}
