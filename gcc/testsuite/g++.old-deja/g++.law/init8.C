// Build don't link: 
// GROUPS passed initialization
// init file
// From: kol@world.std.com (Nikolay Yatsenko)
// Date:     Wed, 27 Jan 1993 16:39:13 -0500
// Subject:  g++ bug
// Message-ID: <199301272139.AA25514@world.std.com>


const int ic = 1;
void f(int& arg)
{ // ERROR - argument 1
        if (arg) ;
}
const int& icr = ic;

int main(void)
{
  f(icr);   // g++ does not give error here// ERROR - .*

  return 0;
}
