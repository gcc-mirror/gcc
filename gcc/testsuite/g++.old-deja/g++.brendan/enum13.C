// Build don't link: 
// GROUPS passed enums
enum COLOR { red, green, blue };
 
struct S {
    COLOR       color:2;
};
 
COLOR color;
S object;
 
void fubar ()
{
    color = object.color;
}
