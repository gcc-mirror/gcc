// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

int i = 3;
void *pv=&i;
const void* pcv=&i;
int main() 
{ 
    pcv = 0 ? pv : pcv;
}
