// Build don't link: 
// GROUPS passed conversions
// cvt file
// Message-Id: <ISHAI.93Mar26102509@cs73.technion.ac.il>
// From: ishai@cs.technion.ac.il (& Ben-Aroya)
// Subject: Type conversion problem.
// Date: Fri, 26 Mar 1993 08:25:09 GMT

typedef int array[10];

void f(array &arg)
{
}

int main()
{
    array var;
    f(var);
}
