// Build don't link: 
// Special g++ Options: -pedantic-errors
// GROUPS passed enums
enum Thing { FIRST, SECOND } ;

int main()
{
    Thing x = FIRST ;
    x = 27 ;          // this line should be a type error.// ERROR - .*
}
